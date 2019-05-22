{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Applicative
import Control.Exception
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

failParse who what = error (T.unpack who ++ " couldn't parse: " ++ T.unpack what)

parseBool "Yes" = True
parseBool "No" = False
parseBool x = failParse "parseBool" x

parseMethod "GET" = Get
parseMethod "POST" = Post
parseMethod x = failParse "parseMethod" x

parseRequiredAccess "" = None
parseRequiredAccess "billing" = Billing
parseRequiredAccess "dns" = Dns
parseRequiredAccess "firewall" = Firewall
parseRequiredAccess "manage_users" = ManageUsers
parseRequiredAccess "provisioning" = Provisioning
parseRequiredAccess "subscriptions" = Subscriptions
parseRequiredAccess "upgrade" = Upgrade
parseRequiredAccess x = failParse "parseRequiredAccess" x

parseParameter x
    | (n : t : ds) <- T.words x
    -- Found three "bugs" in the page! :D
    = let param t' d o = Just (Parameter n o t' (T.unwords ds))
          p = case t of
            "(optional," -> param ParamString (t : ds)
            "Password"   -> param ParamString (t : ds)
            -- Unify int and integer
            "int"         -> param ParamInteger ds
            -- And one non-bug. n:[t] == "No parameters."
            "parameters." -> const Nothing
            _            -> param (parseParamType t) ds
      in p ("optional" `T.isInfixOf` x)
    | otherwise = failParse "parseParameter" x

parseParamType "array" = ParamArray
parseParamType "integer" = ParamInteger
parseParamType "string" = ParamString
parseParamType x = failParse "parseParamType" x

-- | Smoosh whitespace. Probably a better way to do this in base...
parseString = T.intercalate " " . concatMap T.words . T.lines

-- Endpoint

data ParamType = ParamArray  | ParamInteger | ParamString
    deriving (Eq, Show, Ord)

data Parameter = Parameter
    { name :: Text
    , optional :: Bool
    , typ :: ParamType
    , description :: Text
    } deriving (Eq, Show)

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
    , parameters :: [Parameter]
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
        <*> (parseString <$> text "p")
        <*> pure (parseBool key)
        <*> pure (parseMethod method)
        <*> pure (parseRequiredAccess acc)
        <*> pure (Example exReq exResp)
        <*> pure (catMaybes (map parseParameter (T.lines params)))
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
