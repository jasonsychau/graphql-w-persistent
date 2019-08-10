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
import Data.Conduit (sourceToList)
import Data.Text (Text,pack)
import Database.Persist.MySQL (runSqlConn,rawQuery,withMySQLConn,defaultConnectInfo,SqlBackend,ConnectInfo,connectHost,connectPort,connectUser,connectPassword,connectDatabase,fromPersistValue,runMigrationSilent)
import Database.Persist.TH (mkPersist,mkMigrate,sqlSettings,share)
import GraphQL (processQueryString,processQueryData)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] []

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
        -- parse the given query string to make desired query
        (packageObjects,queries) <- processQueryString
            "gqldb_schema.json" qry vars
        -- query
        queryResults <- mapM (mapM runQuery) queries
        -- process data
        processedResults <- processQueryData
            "gqldb_schema.json" packageObjects $ map (map $ map $ map
                (\y -> case (fromPersistValue y :: Either Text Text) of
                    (Right res) -> res
                    (Left res)  -> res)) queryResults
        return processedResults
        where
            runQuery x = sourceToList $ rawQuery (pack x) []
runMySql :: (MonadUnliftIO m) => ConnectInfo -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runMySql conn = runResourceT
              . runNoLoggingT
              . withMySQLConn conn
              . runSqlConn
