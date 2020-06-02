#!/usr/bin/env stack
{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TemplateHaskell
  , FlexibleContexts
#-}
module Lib
    ( runQuery
    ) where

import Conduit (ResourceT,MonadUnliftIO(..),runResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Conduit (sourceToList)
import Data.Text (Text,pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Persist.Postgresql (runSqlConn,rawQuery,withPostgresqlConn,SqlBackend,fromPersistValue,runMigrationSilent)
import Database.Persist.TH (mkPersist,mkMigrate,sqlSettings,share)
import GraphQLdbi (processSchema,processQueryString,processQueryData)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] []

toTxt :: PersistValue -> Text
toTxt (PersistText txt) = txt
toDbl :: PersistValue -> Double
toDbl (PersistDouble d) = d
toDbl (PersistRational r) = (fromRational r)
toInt :: PersistValue -> Int64
toInt (PersistInt64 i) = i
toBool :: PersistValue -> Bool
toBool (PersistBool b) = b
isNull :: PersistValue -> Bool
isNull PersistNull = True
isNull _ = False
runQuery :: String -> String -> IO String
runQuery qry vars =  do
    let connectionString = toStrict $ encodeUtf8 $ fromStrict $ pack ""
    runPostgresql connectionString $ do
        runMigrationSilent migrateAll
        -- get schema data
        schema <- processSchema "gqldb_schema.json"
        -- get the given query string to make desired query
        let (packageObjects,queries) = processQueryString
            schema qry vars
        -- query
        queryResults <- mapM (mapM (mapM runQuery)) queries
        -- process data
        let processedResults = processQueryData toTxt toDbl toInt toBool isNull schema packageObjects queryResults
        return processedResults
        where
            runQuery x = sourceToList $ rawQuery (pack x) []
runPostgresql :: (MonadUnliftIO m) => ByteString -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runPostgresql connstr = runResourceT
                      . runNoLoggingT
                      . withPostgresqlConn connstr
                      . runSqlConn
