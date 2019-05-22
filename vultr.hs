{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import Text.HTML.Scalpel.Core
import Text.Show.Prettyprint
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Selector helpers

t @. cs = AnyTag @: map hasClass cs

div_ = ("div" @.)

id_ k = AnyTag @: ["id" @= T.unpack k]

-- Hodonk parsers

parseBool "Yes" = pure True
parseBool "No" = pure False
parseBool _ = fail "parseBool"

parseMethod "GET" = pure Get
parseMethod "POST" = pure Post
parseMethod _ = fail "parseMethod"

parseRequiredAccess "" = pure None
parseRequiredAccess "billing" = pure Billing
parseRequiredAccess "dns" = pure Dns
parseRequiredAccess "firewall" = pure Firewall
parseRequiredAccess "manage_users" = pure ManageUsers
parseRequiredAccess "provisioning" = pure Provisioning
parseRequiredAccess "subscriptions" = pure Subscriptions
parseRequiredAccess "upgrade" = pure Upgrade
parseRequiredAccess _ = fail "parseRequiredAccess"

-- | Smoosh whitespace. Probably a better way to do this in base...
parseString = pure . T.intercalate " " . concatMap T.words . T.lines

-- Endpoint

data Method = Post | Get
    deriving (Eq, Show)

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

data Endpoint = Endpoint
    { path :: Text
    , description :: Text
    , needsAPIKey :: Bool
    , method :: Method
    , requiredAccess :: RequiredAccess
    , example :: Example
    , parameters :: Text
    } deriving (Eq, Show)

data Example = Example { request :: Text, response :: Text }
    deriving (Eq, Show)

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
        <$> text ("h3" // "a")
        <*> (parseString =<< text "p")
        <*> parseBool key
        <*> parseMethod method
        <*> parseRequiredAccess acc
        <*> pure (Example exReq exResp)
        <*> pure params
    where
    scrape2ndTd =
        inSerial (replicateM 2 (seekNext (pure ())) >> seekNext (text "td"))

-- ApiGroup

data ApiGroup = ApiGroup { name :: Text, endpoints :: [Endpoint] }
    deriving (Eq, Show)

scrapeApiGroup :: Scraper Text ApiGroup
scrapeApiGroup =
    ApiGroup <$> (text "h2") <*> chroots ("div" `atDepth` 1) scrapeEndpoint
    `guardedBy` not . null . endpoints

guardedBy :: (Monad f, Alternative f) => f a -> (a -> Bool) -> f a
x `guardedBy` b = do
    x' <- x
    x' <$ guard (b x')
infixl 1 `guardedBy`


-- main helpers

apiGroupRoot = div_ ["main-content"] // div_ ["content-row"]

-- | Like scrape, but crap
scrap :: Scraper Text a -> IO a
scrap s = fromJust . flip scrapeStringLike s <$> T.readFile "vultr.html"

main =
    prettyPrint . take 5
        =<< scrap (chroots apiGroupRoot scrapeApiGroup)
