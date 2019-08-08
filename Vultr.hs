{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Vultr where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Foldable
import Data.HashMap.Strict (elems, keys, singleton)
import Data.HashSet (HashSet)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector ((!?))
import GHC.Generics
import Servant.API
import Text.HTML.Scalpel.Core
import Text.Pretty.Simple
import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Vector as V

-------------------
-- Selector helpers
-------------------

t @. cs = AnyTag @: map hasClass cs

div_ = ("div" @.)

id_ k = AnyTag @: ["id" @= T.unpack k]

-- | parse helper
failParse who what = error (T.unpack who ++ " couldn't parse: " ++ T.unpack what)

-----------------
-- Hodonk parsers
-----------------

-- | "Yes", "No"
parseBool "Yes" = True
parseBool "yes" = True
parseBool "No" = False
parseBool "no" = False
parseBool x = failParse "parseBool" x

-- | Parse a thing like "GET"; very straightforward
parseMethod "GET" = Get
parseMethod "POST" = Post
parseMethod x = failParse "parseMethod" x

-- | Parse a thing like "billing"; very straightforward
parseAcl "" = None
parseAcl "billing" = Billing
parseAcl "dns" = Dns
parseAcl "firewall" = Firewall
parseAcl "manage_users" = ManageUsers
parseAcl "provisioning" = Provisioning
parseAcl "subscriptions" = Subscriptions
parseAcl "upgrade" = Upgrade
parseAcl "support" = Support
parseAcl x = failParse "parseAcl" x

-- | Parse a parameter line, which usually looks like
--
-- @name string (optional) New name for the SSH key@
parseParameter x
    | x == "No parameters." = Nothing
    | (nam : typ : desc) <- T.words x
    -- Found two "bugs" in the page! :D Most param lines have the form
    --
    -- name type description
    --
    -- But a few are missing their type. We manually patch it in with static
    -- matches.
    = let param t' d o = Just (Parameter nam o t' (T.unwords desc))
          p = case typ of
            "(optional," -> param ParamString (typ : desc)
            "Password"   -> param ParamString (typ : desc)
            _            -> param (parseParamType typ) desc
      in p ("optional" `T.isInfixOf` x)
    | otherwise = failParse "parseParameter" x

-- | E.g. "array"
parseParamType "array" = ParamArray
-- Both "integer" and "int" are used.
parseParamType "integer" = ParamInteger
parseParamType "int" = ParamInteger
parseParamType "string" = ParamString
parseParamType x = failParse "parseParamType" x

-- | Smoosh whitespace. Seems like a hack.
parseString = T.unwords . T.words

parsePath :: Text -> Path
parsePath  = Path . filter (not . T.null) . T.splitOn "/"

-- | Parse Responses. Besides hodonk parsers like the above, we get a little
-- more sophisticated by attempting JSON parses of the example text to ascertain
-- the type.
parseResponse :: Text -> Response
parseResponse "No response, check HTTP result code." = NoResponse
parseResponse " " = NoResponse -- classy
parseResponse "\n" = NoResponse -- extra classy
parseResponse x
    | Just t <- parseJson x = t
    | otherwise = Wat x

-------------------------
-- Parsing response types
-------------------------
--
-- There are response types that have to be reverse engineered. Then, we have
-- to figure out which endpoints return what sorts of responses. This is a
-- bit of a chicken and egg problem. The first plan was to hand-write type
-- declarations and use JSON parsing to sniff response types, but that did not
-- solve the circular dependency. The new proposal is to sniff the types without
-- intermediate type definitions, just looking at the JSON itself.
--
-------------------------

newtype TypeSniffer = TypeSniffer (HashSet Text)
    deriving (Eq)

-- | For a given response, these are the fields we expect to find.
responseSniffer :: ResponseTerm -> TypeSniffer
responseSniffer = TypeSniffer . Set.fromList . \case
    User -> ["USERID", "name", "email", "api_enabled", "acls"]
    UserRef -> ["USERID", "api_key"]
    Script ->
        ["SCRIPTID", "date_created", "date_modified", "name", "type", "script"]
    ScriptRef -> ["SCRIPTID"]
    SshKey -> ["SSHKEYID", "date_created", "name", "ssh_key"]
    SshKeyRef -> ["SSHKEYID"]
    Snapshot ->
        ["SNAPSHOTID", "date_created", "description", "size", "status", "OSID", "APPID"]
    SnapshotRef -> ["SNAPSHOTID"]
    Ip6Reverse -> ["ip", "reverse"]
    PrivateNetwork -> ["NETWORKID", "mac_address", "ip_address"]
    OsTarget -> ["OSID", "name", "arch", "family", "windows", "surcharge"]
    -- The ip types are sadly all pretty close
    Ip6 -> ["ip", "network", "network_size", "type"]
    Ip4 -> ["ip", "netmask", "gateway", "type", "reverse"]
    Ip4BareMetal -> ["ip", "netmask", "gateway", "type"]
    -- >_<
    Server ->
        [ "SUBID"
        , "os"
        , "ram"
        , "disk"
        , "main_ip"
        , "vcpu_count"
        , "location"
        , "DCID"
        , "default_password"
        , "date_created"
        , "pending_charges"
        , "status"
        , "cost_per_month"
        , "current_bandwidth_gb"
        , "allowed_bandwidth_gb"
        , "netmask_v4"
        , "gateway_v4"
        , "power_status"
        , "server_state"
        , "VPSPLANID"
        , "v6_main_ip"
        , "v6_network_size"
        , "v6_network"
        , "v6_networks"
        , "label"
        , "internal_ip"
        , "kvm_url"
        , "auto_backups"
        , "tag"
        , "OSID"
        , "APPID"
        , "FIREWALLGROUPID"
        ]
    IsoStatus -> ["state", "ISOID"]
    UserData -> ["userdata"]
    AppInfo -> ["app_info"]
    ServerRef -> ["SUBID"]
    Bandwidth -> ["incoming_bytes", "outgoing_bytes"]
    BackupSchedule -> ["enabled", "cron_type", "next_scheduled_time_utc", "hour", "dow", "dom"]
    AppTarget -> ["APPID", "name", "short_name", "deploy_name", "surcharge"]
    ReservedIp -> ["SUBID", "DCID", "ip_type", "subnet", "subnet_size", "label", "attached_SUBID"]
    Region -> ["DCID", "name", "country", "continent", "state", "ddos_protection", "block_storage", "regioncode"]
    -- All the vps plans may have an optional field, "deprecated". These 'plan'
    -- types are therefore fragile! And possibly redundant!
    Vdc2Plan -> ["VPSPLANID", "name", "vcpu_count", "ram", "disk", "bandwidth", "price_per_month", "plan_type"]
    VpsPlan -> ["VPSPLANID", "name", "vcpu_count", "ram", "disk", "bandwidth", "price_per_month", "windows", "plan_type", "available_locations"]
    Os -> ["OSID", "name", "arch", "family", "windows"]
    NetworkRef -> ["NETWORKID"]
    PublicIso -> ["ISOID", "name", "description"]
    AccountIso -> ["ISOID", "date_created", "filename", "size", "md5sum", "sha512sum", "status"]
    FirewallGroup -> ["FIREWALLGROUPID", "description", "date_created", "date_modified", "instance_count", "rule_count", "max_rule_count"]
    FirewallGroupRef -> ["FIREWALLGROUPID"]
    SoaInfo -> ["nsprimary", "email"]
    Domain -> ["domain", "date_created"]
    BlockStorage -> ["SUBID", "date_created", "cost_per_month", "status", "size_gb", "DCID", "attached_to_SUBID", "label"]
    BareMetal -> ["SUBID", "os", "ram", "disk", "main_ip", "cpu_count", "location", "DCID", "default_password", "date_created", "status", "netmask_v4", "gateway_v4", "METALPLANID", "v6_networks", "label", "tag", "OSID", "APPID"]
    Backup -> ["BACKUPID", "date_created", "description", "size", "status"]
    AuthInfo -> ["acls", "email", "name"]
    Account -> ["balance", "pending_charges", "last_payment_date", "last_payment_amount"]
    DnsSec -> ["dnssec*mogrify"]
    FirewallRule -> ["rulenumber", "action", "protocol", "port", "subnet", "subnet_size", "notes"]
    FirewallRuleRef -> ["rulenumber"]
    AccountIsoRef -> ["ISOID"]
    Network -> ["DCID", "NETWORKID", "date_created", "description", "v4_subnet", "v4_subnet_mask"]
    BareMetalPlan -> ["METALPLANID", "name", "cpu_count", "ram", "disk", "bandwidth_tb", "price_per_month", "plan_type", "deprecated", "available_locations"]
    DnsRecord -> ["type", "name", "data", "priority", "RECORDID", "ttl"]

-- | Sniff out response types based on the json object keys
sniffType :: Value -> Maybe ResponseTerm
sniffType (Object x) =
    let input = TypeSniffer (Set.fromList (keys x))
    in
    case
    filter
        ((== input) . snd)
        (map (id &&& responseSniffer) [minBound ..])
    of
    [] -> Nothing
    [(x,_)] -> Just x
    ms -> (error . TL.unpack . pString)
        ("The type sniffer found >1 match for an object.\n****  Object: "
        <> show x <>"\n****  Matches these: " <> show (map fst ms))
sniffType _ = Nothing

-- | But! Some values do not submit to simple sniffying. To deal with them, we
-- will add some fake wrapper based on perilous, hodonk parsing.
mogrify :: Value -> Value
mogrify v@(String t)
    | T.isPrefixOf "example.com IN DNSKEY" t
    -- Fragile: relies on the first entry of the DnsSec entry being exactly what
    -- it is. (Which, unless DNS entries can elide their TTL, is ill-formatted.)
    = Object (singleton "dnssec*mogrify" v)
mogrify v = v

-- | Use speculative json parsing to figure out the type of a response.
--
-- We look for a list of ints at the top level, since it otherwise conflicts
-- with ResponseList (and a bare int is usually misleading to the rest of the
-- sniffers).
parseJson :: Text -> Maybe Response
parseJson =
    let
        ifromJSON' :: FromJSON a => Value -> Maybe a
        ifromJSON' x = case ifromJSON x of
            ISuccess a -> Just a
            _ -> Nothing

        parseValue' blob =
            ResponseInts <$ ifromJSON' @ [Int] blob
            <|> parseValue blob
    in
    parseValue' <=< decodeStrict . encodeUtf8

tryListParse :: Value -> Maybe Response
tryListParse (Array x) = ListOf <$> (parseValue =<< x !? 0)
tryListParse _ = Nothing

-- | See if we can't find a Response out of a Value.
parseValue :: Value -> Maybe Response
parseValue blob =
    getFirst (mconcat (fmap First (
        [ Term <$> sniffType (mogrify blob)
        , tryListParse blob
        , tryKeyedObjectParse blob
        ])))

-- | Ok, some responses are returned as a map keyed by ID. We need to pick one
-- and inspect it.
--
-- MOST instances of a keyed response are redundant: the item with key k also
-- has k as one of its fields. I must use my eyeballs to spot the exceptions.
-- They are: Ip4BareMetal (which shows up as KeyedResponse (ListOf (Term Ip4BareMetal)),
-- which might be a useful trend?), Ip6 (yes!), Ip4, Ip6Reverse. So, yes,
-- the Ip* response types are the exceptions, all of which are keyed on SUBID,
-- and all of which are a KeyedResponse of a List.
tryKeyedObjectParse :: Value -> Maybe Response
tryKeyedObjectParse (Object x) =
    let get1 = headMay . elems
        headMay (x:_) = Just x
        headMay _ = Nothing
    in KeyedResponse <$> (parseValue =<< get1 x)
tryKeyedObjectParse _ = Nothing

-------------
-- Data types
-------------

-- | This is the sum of possible response types from querying endpoints.
--
-- Not to be confused with the actual response types, i.e. 'User', this type is
-- used to annotate Endpoint values. It is implemented as a grammar so that the
-- real types (the "terms") can be specified in a total way as @[minBound ..]@.
data Response
    = NoResponse
    | KeyedResponse Response
    -- ^ This is a 'list' of values, redundantly keyed on their id.
    | ListOf Response
    | Term ResponseTerm
    -- ^ A "real" response term(inal). Except for bare ints, below.
    | ResponseInts
    -- ^ A list of ints
    | Wat Text
    -- ^ Things we fail to understand.
    deriving (Eq, Show, Ord)

-- | The list of "real" response term(inal)s.
data ResponseTerm
    = Account
    | AccountIso
    | AccountIsoRef
    | AppInfo
    | AppTarget
    | AuthInfo
    | Backup
    | BackupSchedule
    | Bandwidth
    | BareMetal
    | BareMetalPlan
    | BlockStorage
    | DnsRecord
    | DnsSec
    | Domain
    | FirewallGroup
    | FirewallGroupRef
    | FirewallRule
    | FirewallRuleRef
    | Ip4
    | Ip4BareMetal
    | Ip6
    | Ip6Reverse
    | IsoStatus
    | Network
    | NetworkRef
    | Os
    | OsTarget
    | PrivateNetwork
    | PublicIso
    | Region
    | ReservedIp
    | Script
    | ScriptRef
    | Server
    | ServerRef
    | Snapshot
    | SnapshotRef
    | SoaInfo
    | SshKey
    | SshKeyRef
    | User
    | UserData
    | UserRef -- ^ returns the API key as well
    | Vdc2Plan -- ^ No idea what this is just yet.
    | VpsPlan
    deriving (Eq, Show, Ord, Bounded, Enum)

-- | Just taking what we can get. This is not a rich transformation.
data ParamType = ParamArray  | ParamInteger | ParamString
    deriving (Eq, Show, Ord)

-- | More workable version of the parameter lines, which start out like
--
-- @name string (optional) New name for the SSH key@
data Parameter = Parameter
    { pname :: Text
    , optional :: Bool
    , typ :: ParamType
    , pdescription :: Text
    } deriving (Eq, Show, Ord)

-- | Ez
data Method = Post | Get
    deriving (Eq, Show)

-- | I suppose this is the beginning of Vultr's fine-grained access control.
data Acl
    = None
    | Billing
    | Dns
    | Firewall
    | ManageUsers
    | Provisioning
    | Subscriptions
    | Upgrade
    | Support
    deriving (Eq, Show, Ord, Generic)

-- | Converts camelcase to snakecase in constructor tags.
instance FromJSON Acl where
    parseJSON = genericParseJSON defaultOptions {
        constructorTagModifier = map toLower . camelTo2 '_'
        }

newtype Path = Path [Text]
    deriving (Eq, Show)

-- | Behold its glory!!
data Endpoint = Endpoint
    { path :: Path
    , edescription :: Text
    , needsAPIKey :: Bool
    , method :: Method
    , requiredAccess :: Acl
    , example :: Example
    , parameters :: [Parameter]
    , responseType :: Response
    } deriving (Eq, Show)

-- | Useful for test
nullEP = Endpoint (Path []) "" False Get None (Example "" "") [] NoResponse

-- | Simple wrapper over endpoint examples
data Example = Example { request :: Text, response :: Text }
    deriving (Eq, Show)

-- | Groups of endpoints, kept around for documentation purposes.
data ApiGroup = ApiGroup { name :: Text, endpoints :: [Endpoint] }
    deriving (Eq, Show)

-----------
-- Scrapers
-----------

-- | Top-level scraper for Endpoint
scrapeEndpoint :: Scraper Text Endpoint
scrapeEndpoint = do
    [key, method, acc] <-
        chroot "table" (inSerial $ do
            key <- seekNext (chroot "tr" scrape2ndTd)
            method <- seekNext (chroot "tr" scrape2ndTd)
            acc <-
                seekNext (chroot "tr" scrape2ndTd)
                <|> pure ""
            pure [key, method, acc]
        )
    -- These ones aren't very distinguished
    [exReq, exResp, params] <- texts "code"
    let ex = Example exReq exResp
    Endpoint
        <$> (parsePath <$> text ("h3" // "a"))
        <*> (parseString <$> text "p")
        <*> pure (parseBool key)
        <*> pure (parseMethod method)
        <*> pure (parseAcl acc)
        <*> pure ex
        <*> pure (mapMaybe parseParameter (T.lines params))
        <*> pure (parseResponse exResp)
    where
    scrape2ndTd =
        inSerial (replicateM_ 2 (seekNext (pure ())) >> seekNext (text "td"))

-- | Scrape API groups
scrapeApiGroup :: Scraper Text ApiGroup
scrapeApiGroup =
    ApiGroup <$> text "h2" <*> chroots ("div" `atDepth` 1) scrapeEndpoint
    `guardedBy` not . null . endpoints

-- helpers

-- | Helper that seems pretty natural to me
guardedBy :: (Monad f, Alternative f) => f a -> (a -> Bool) -> f a
x `guardedBy` b = do
    x' <- x
    x' <$ guard (b x')
infixl 1 `guardedBy`

apiGroupRoot = div_ ["main-content"] // div_ ["content-row"]

-- | Like scrape, but crap (is partial)
scrap :: Scraper Text a -> IO a
scrap s = fromJust . flip scrapeStringLike s <$> T.readFile "vultr.html"

muhEndpointGroups = scrap (chroots apiGroupRoot scrapeApiGroup)

main =
    pPrint
        . filter (isWat . snd . snd)
        -- . filter ((/= NoResponse) . snd. snd)
        -- . filter (((&&) <$> (not . isWat) <*> (/= NoResponse)) . snd . snd)
        . map (path &&& edescription &&& (parseResponse . response . example)) . concatMap endpoints
        =<< muhEndpointGroups
    where
    isWat (Wat _) = True
    isWat _ = False

newtype ApiKey = ApiKey Text
    deriving (Eq, Show)

-- Building AST elements.
apiKeyHeader Endpoint { needsAPIKey }
    | needsAPIKey = [t|Header "API-Key" ApiKey|]
    | otherwise = [t|Header' '[Optional, Strict] "API-Key" ApiKey|]
