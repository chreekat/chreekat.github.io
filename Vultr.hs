{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module Vultr where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import Servant.API
import Text.HTML.Scalpel.Core
import Text.Show.Prettyprint
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
parseBool "No" = False
parseBool x = failParse "parseBool" x

-- | Parse a thing like "GET"; very straightforward
parseMethod "GET" = Get
parseMethod "POST" = Post
parseMethod x = failParse "parseMethod" x

-- | Parse a thing like "billing"; very straightforward
parseRequiredAccess "" = None
parseRequiredAccess "billing" = Billing
parseRequiredAccess "dns" = Dns
parseRequiredAccess "firewall" = Firewall
parseRequiredAccess "manage_users" = ManageUsers
parseRequiredAccess "provisioning" = Provisioning
parseRequiredAccess "subscriptions" = Subscriptions
parseRequiredAccess "upgrade" = Upgrade
parseRequiredAccess x = failParse "parseRequiredAccess" x

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

-------------
-- Data types
-------------

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
data RequiredAccess
    = None
    | Billing
    | Dns
    | Firewall
    | ManageUsers
    | Provisioning
    | Subscriptions
    | Upgrade
    deriving (Eq, Show)

newtype Path = Path [Text]
    deriving (Eq, Show)

-- | Behold its glory!!
data Endpoint = Endpoint
    { path :: Path
    , edescription :: Text
    , needsAPIKey :: Bool
    , method :: Method
    , requiredAccess :: RequiredAccess
    , example :: Example
    , parameters :: [Parameter]
    } deriving (Eq, Show)

-- | Useful for test
nullEP = Endpoint (Path []) "" False Get None (Example "" "") []

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
    Endpoint
        <$> (parsePath <$> text ("h3" // "a"))
        <*> (parseString <$> text "p")
        <*> pure (parseBool key)
        <*> pure (parseMethod method)
        <*> pure (parseRequiredAccess acc)
        <*> pure (Example exReq exResp)
        <*> pure (catMaybes (map parseParameter (T.lines params)))
    where
    scrape2ndTd =
        inSerial (replicateM 2 (seekNext (pure ())) >> seekNext (text "td"))

-- | Scrape API groups
scrapeApiGroup :: Scraper Text ApiGroup
scrapeApiGroup =
    ApiGroup <$> (text "h2") <*> chroots ("div" `atDepth` 1) scrapeEndpoint
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

main =
    prettyPrint . head . endpoints . head . drop 3
        =<< scrap (chroots apiGroupRoot scrapeApiGroup)

newtype ApiKey = ApiKey Text
    deriving (Eq, Show)

-- Building AST elements.
apiKeyHeader (Endpoint { needsAPIKey })
    | needsAPIKey = [t|Header "API-Key" ApiKey|]
    | otherwise = [t|Header' '[Optional, Strict] "API-Key" ApiKey|]
