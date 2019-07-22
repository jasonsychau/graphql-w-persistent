{-# LANGUAGE
    DeriveGeneric
  , NamedFieldPuns
#-}

module Main where

import AWS.Lambda.Runtime (ioRuntime)
import Data.Aeson         (FromJSON, ToJSON)
import Data.Either        (Either(Right))
import GHC.Generics       (Generic)
import Lib                (runQuery)


data Payload  = Payload { query     :: String
                        , variables :: String
                        } deriving Generic
instance FromJSON Payload

myHandler :: Payload -> IO (Either String String)
myHandler Payload { query, variables } = do
    res <- runQuery query variables
    return $ Right res

main :: IO ()
main = ioRuntime myHandler
