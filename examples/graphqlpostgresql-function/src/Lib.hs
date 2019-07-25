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
import GraphQL (processQueryString,processQueryData)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] []

runQuery :: String -> String -> IO String
runQuery qry vars =  do
    let connectionString = toStrict $ encodeUtf8 $ fromStrict $ pack ""
    runPostgresql connectionString $ do
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
                    _ -> "error")) queryResults
        return processedResults
        where
            runQuery x = sourceToList $ rawQuery (pack x) []
runPostgresql :: (MonadUnliftIO m) => ByteString -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runPostgresql connstr = runResourceT
                      . runNoLoggingT
                      . withPostgresqlConn connstr
                      . runSqlConn
