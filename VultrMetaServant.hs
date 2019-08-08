{-# LANGUAGE OverloadedStrings #-}

-- | Gonna generate me some Haskell code, string-wise. Template Haskell sux
module VultrMetaServant where

import Data.Text (Text)
import Servant.API
import Text.Pretty.Simple
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Vultr hiding (main)

-- All we need is a printer. We can print Endpoint and basically we're done.
printEndpoint :: Endpoint -> Text
printEndpoint (Endpoint p d a m r e ps res)
    = printPath p
    <:> printAddDescription d
    <:> printApiHeader a
    <:> printMethod res m
        <> " '[JSON] "
        <> printResponse res

printResponse NoResponse = "NoContent"
printResponse ResponseInts = "[Int]"
printResponse (ListOf b) = "[" <> printResponse b <> "]"
printResponse (KeyedResponse (ListOf b)) = "(Map SubId " <> printResponse b <> ")"
printResponse (KeyedResponse b) = "(Map " <> printRefType b <> " " <> printResponse b <> ")"
printResponse (Term t) = T.pack (show t)


printRefType (Term b) = T.pack (show b) <> "Id"

a <:> b = a <> " :> " <> b

printMethod :: Response -> Method -> Text
printMethod NoResponse = (<> "NoContent") . T.pack . show
printMethod _ = T.pack . show

apiKeyTy = "ApiKey"

printPath :: Path -> Text
printPath (Path ps) = T.intercalate " :> " (map stringLit ps)

printApiHeader :: Bool -> Text
printApiHeader True = "Header \"API-Key\" " <> apiKeyTy
printApiHeader False = "Header' '['Strict, 'Optional] \"API-Key\" " <> apiKeyTy

-- | Add a description to a printed thing.
printAddDescription :: Text -> Text
printAddDescription t = "Description " <> stringLit t

main =  do
    pPrint x
    T.putStrLn (printEndpoint x)
    where
    x =nullEP
        { path = Path (T.words "v1 backup list")
        , edescription = "List all backups on the current account. Required access: Subscriptions."
        , needsAPIKey = True
        , method = Get
        , responseType = KeyedResponse (Term Backup)
        }

stringLit t = "\"" <> t <> "\""
{-
type A = "v1" :> "backup" :> "list"
    :> $(apiKeyHeader (nullEP { needsAPIKey = True }))
    :> (Description "Filter result set to only contain backups of this subscription object."
        :> QueryParam' '[Optional, Strict] "SUBID" SubId)
    :> (Description "Filter result set to only contain this backup"
        :> QueryParam' '[Optional, Strict] "BACKUPID" BackupId)
    :> Description "List all backups on the current account. Required access: Subscriptions."
    :> Get '[JSON] [(BackupId, Backup)]

-}
