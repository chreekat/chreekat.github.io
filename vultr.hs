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
    { name :: EndpointName
    , path :: Text
    , needsAPIKey :: Bool
    , description :: Text
    , method :: Method
    , requiredAccess :: RequiredAccess
    , example :: EndpointExample
    } deriving (Eq, Show)

newtype EndpointName = EndpointName Text
    deriving (Eq, Show)

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
    { ref :: Text
    , route :: Text
    }
    deriving (Eq, Show)

t @. cs = AnyTag @: map hasClass cs

div_ = ("div" @.)

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

-- main = prettyPrint =<< (fromJust . flip scrapeStringLike (html (AnyTag @: ["id" @= "sshkey_destroy"])) <$> h)
main = mapM_ prettyPrint =<< (fromJust . flip scrapeStringLike scrapeGroups <$> h)
