#!/usr/bin/env stack
{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TemplateHaskell
  , FlexibleContexts
  , TypeFamilies
#-}
module Lib
    ( runQuery
    ) where

import Conduit (ResourceT,MonadUnliftIO(..),runResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Conduit (sourceToList)
import Data.Text (Text,pack)
import Database.Persist.MySQL (
  runSqlConn,
  rawQuery,
  withMySQLConn,
  defaultConnectInfo,
  SqlBackend,
  ConnectInfo,
  connectHost,
  connectPort,
  connectUser,
  connectPassword,
  connectDatabase,
  fromPersistValue,
  runMigrationSilent,
  PersistValue(PersistText,
    PersistDouble,
    PersistRational,
    PersistInt64,
    PersistBool,
    PersistNull))
import Database.Persist.TH (mkPersist,mkMigrate,sqlSettings,share)
import GraphQLdbi (processSchema,processQueryString,processQueryData)
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)

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
    let connectionInfo = defaultConnectInfo {
        connectHost = "",
        connectPort = 3306,
        connectUser = "",
        connectPassword = "",
        connectDatabase = ""
    }
    runMySql connectionInfo $ do
        runMigrationSilent migrateAll
        -- get schema data
        schema <- liftIO $ processSchema "gqldb_schema.json"
        -- parse the given query string to make desired query
        let (packageObjects,queries) = processQueryString schema qry vars
        -- query
        queryResults <- mapM (mapM (mapM runQuery)) queries
        -- process data
        let processedResults = processQueryData toTxt toDbl toInt toBool isNull schema packageObjects queryResults
        return processedResults
        where
            runQuery x = sourceToList $ rawQuery (pack x) []
runMySql :: (MonadUnliftIO m) => ConnectInfo -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runMySql conn = runResourceT
              . runNoLoggingT
              . withMySQLConn conn
              . runSqlConn
