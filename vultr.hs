{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-
import AllThatServantShit
type VultrAPI
    = "v1" :> VultrAPI1

type VultrAPI1
    = "account" :> "info" :> Get '[JSON] AccountInfo
    = "app" :> "list" :> Get '[JSON] AppList
    = "auth" :> "info" :> Get '[JSON] AuthInfo

-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Text.HTML.Scalpel.Core
import Text.Show.Prettyprint
import qualified Data.Set as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

{-
data Endpoint = Endpoint
    { path :: Text
    , description :: Text
    , needsAPIKey :: Bool
    , method :: Method
    , requiredAccess :: RequiredAccess
    , example :: EndpointExample
    } deriving (Eq, Show)

data RequiredAccess = Subscriptions | Provisioning | Upgrade | Dns
    deriving (Eq, Show)

data EndpointExample = EndpointExample
    deriving (Eq, Show)

data Method = Get | Put
    deriving (Eq, Show)
-}


t @. cs = AnyTag @: map hasClass cs

div_ = ("div" @.)

id_ k = AnyTag @: ["id" @= T.unpack k]


data Endpoint = Endpoint
    { path :: Text
    , description :: Text
    , needsAPIKey :: Text -- Bool
    , method :: Text -- Method
    , requiredAccess :: Text -- RequiredAccess
    } deriving (Eq, Ord, Show)

scrapeEndpoint :: Scraper Text Endpoint
scrapeEndpoint = do
    [key, typ, acc] <-
        chroot "table" (inSerial $ do
            key <- seekNext (chroot "tr" scrape2ndTd)
            typ <- seekNext (chroot "tr" scrape2ndTd)
            acc <-
                seekNext (chroot "tr" scrape2ndTd)
                <|> pure ""
            pure [key, typ, acc]
        )
    Endpoint
        <$> text ("h3" // "a")
        <*> text "p"
        <*> pure key
        <*> pure typ
        <*> pure acc
    where
    scrape2ndTd =
        inSerial (replicateM 2 (seekNext (pure ())) >> seekNext (text "td"))

data ApiGroup = ApiGroup { name :: Text, endpoints :: [Endpoint] }
    deriving (Eq, Ord, Show)

guardedBy :: (Monad f, Alternative f) => f a -> (a -> Bool) -> f a
x `guardedBy` b = do
    x' <- x
    x' <$ guard (b x')
infixl 1 `guardedBy`

scrapeApiGroup :: Scraper Text ApiGroup
scrapeApiGroup =
    ApiGroup <$> (text "h2") <*> chroots "div" scrapeEndpoint
    `guardedBy` not . null . endpoints

apiGroupRoot = div_ ["main-content"] // div_ ["content-row"]

-- | Like scrape, but crap
scrap :: Scraper Text a -> IO a
scrap s = fromJust . flip scrapeStringLike s <$> T.readFile "vultr.html"

main = prettyPrint . M.fromList . take 3 =<< scrap (chroots apiGroupRoot scrapeApiGroup)
    where
    sel = div_ ["main-content"] // div_ ["content-row"]
--main = mapM_ prettyPrint =<< (fromJust . flip scrapeStringLike scrapeGroups <$> h)
--main = prettyPrint =<< (fromJust . flip scrapeStringLike (html (AnyTag @: ["id" @= "sshkey_destroy"])) <$> h)
