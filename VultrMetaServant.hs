{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
module VultrMetaServant where

import Language.Haskell.TH hiding (Strict)
import Servant.API
import Data.Text (Text)

import Vultr

--------------
-- Known types
--------------

------------------
-- Generated types
------------------

-- Need to generate types for the parameters, like
newtype SubId = SubId Int
newtype BackupId = BackupId Text
newtype ServerTag = ServerTag Text

-- Also need types for the objects sent and received, like
data Backup = Backup
    { dateCreated :: Text -- FIXME: UTCTime
    , description :: Text
    , size :: Int
    , status :: BackupStatus
    } deriving (Eq, Show)

-- | We don't exhaustively know the return types, so we need to leave an escape
-- hatch. (FIXME: Bytes, not Text)
data BackupStatus
    = Complete
    | Unrecognized Text
    deriving (Eq, Show)

--------------------------------
-- Manual example of an endpoint
--------------------------------

-- The parts of Endpoint relevant to Servant:
--
-- path :: Path
-- needsAPIKey :: Bool
-- method :: Method
-- requiredAccess :: RequiredAccess
-- parameters :: [Parameter]
-- the response (which is part of Example)

-- metaMethod :: Method -> _
-- metaMethod
--

type A = "v1" :> "backup" :> "list"
    :> $(apiKeyHeader (Endpoint { needsAPIKey = True }))
    :> (Description "Filter result set to only contain backups of this subscription object."
        :> QueryParam' '[Optional, Strict] "SUBID" SubId)
    :> (Description "Filter result set to only contain this backup"
        :> QueryParam' '[Optional, Strict] "BACKUPID" BackupId)
    :> Description "List all backups on the current account. Required access: Subscriptions."
    :> Get '[JSON] [(BackupId, Backup)]


