{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

{-
import AllThatServantShit
type VultrAPI
    = "v1" :> VultrAPI1

type VultrAPI1
    = "account" :> "info" :> Get '[JSON] AccountInfo
    = "app" :> "list" :> Get '[JSON] AppList
    = "auth" :> "info" :> Get '[JSON] AuthInfo

-- runX $ doc >>> css ".page-main .main-sidebar ul.nav-sidebar > li.dropdown.header ~ li > a" >>> getAttrValue "href"
--
-- runX $ doc >>> css "a[href~=\"#os\"]"
-- runX $ doc >>> css "#os"


So yah, I can get all the ids for groups and all the ids for individual paths.
Then I can group data. Then I can get endpoint data. Then I can combine the
data. Data for everyone. Data.

-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Text.HTML.Scalpel.Core
import Text.Show.Prettyprint
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

data ApiGroup = ApiGroup
    { name :: Text
    , ref :: Text
    , endpoints :: [EndpointRef]
    } deriving (Eq, Show)

data EndpointRef = EndpointRef
    { eref :: Text
    , route :: Text
    }
    deriving (Eq, Show)

t @. cs = AnyTag @: map hasClass cs

div_ = ("div" @.)

id_ k = AnyTag @: ["id" @= T.unpack k]

h = T.readFile "vultr.html"

-- | Get the API groups from the table of contents in the sidebar
scrapeGroups :: Scraper Text [ApiGroup]
scrapeGroups =
    -- The first three matches are bogus introductory links
    drop 3 <$>
    chroots
        (div_ ["page-main"]
            // div_ ["main-sidebar"]
            // "ul" @. ["nav-sidebar"]
            // "li" @. ["dropdown"])
        scrapeGroup
    where
    scrapeGroup =
        ApiGroup
            <$> text "a"
            <*> attr "href" "a"
            <*> chroots ("ul" @. ["nav"] // "li") scrapeRef
    scrapeRef = EndpointRef <$> attr "href" "a" <*> text "a"

data Endpoint = Endpoint
    { path :: Text
    , description :: Text
    , needsAPIKey :: Text -- Bool
    , method :: Text -- Method
    , requiredAccess :: Text -- RequiredAccess
    } deriving (Eq, Show)

scrapeEndpoint :: Text -> Scraper Text Endpoint
scrapeEndpoint k = chroot (id_ k) $ do
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
    scrape2ndTd = inSerial (replicateM 2 (seekNext (pure ())) >> seekNext (text "td"))

--main = prettyPrint =<< (fromJust . flip scrapeStringLike (scrapeEndpoint "sshkey_destroy") <$> h)
--main = mapM_ prettyPrint =<< (fromJust . flip scrapeStringLike scrapeGroups <$> h)
--main = prettyPrint =<< (fromJust . flip scrapeStringLike (html (AnyTag @: ["id" @= "sshkey_destroy"])) <$> h)
main = do
    h' <- h
    let proc = fromJust . scrapeStringLike h'
        groups = proc scrapeGroups -- fromJust . flip scrapeStringLike scrapeGroups <$> h
        eps = (concatMap (map (proc . scrapeEndpoint . T.drop 1 . eref))) (map endpoints groups)
        --eps = (concatMap (map (T.take 1 . eref))) (map endpoints groups)
    mapM_ prettyPrint (take 5 eps)
