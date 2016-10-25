{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application (
  app
) where

import Prelude hiding (id)

import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import Data.Default.Class (def)
import Data.List (sort)
import Data.Pool (Pool, withResource)
import Data.Text.Lazy (Text)
import Database.MySQL.Simple (Connection, Only(..), query_, execute)
import GHC.Generics (Generic)
import Network.HTTP.Types (ok200, notFound404, notImplemented501, StdMethod(HEAD))
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.RequestLogger (Destination(Handle),
  mkRequestLogger, RequestLoggerSettings(destination, outputFormat),
  OutputFormat(CustomOutputFormat))
import Network.Wai.Middleware.Static (addBase, hasPrefix, staticPolicy, (>->))
import System.IO (stderr)
import Web.Scotty (ScottyM, ActionM, middleware, json, file, addroute, get,
  delete, status, text, param, scottyApp)
import qualified Data.HashMap.Lazy as HM
import qualified Network.Wai.Middleware.Gzip as Gzip

import LogFormat (logFormat)

type Pools = HM.HashMap Text (Pool Connection)

app :: Pools -> FilePath -> IO Application
app ps f = do
  logger <- mkRequestLogger def{ destination = Handle stderr
                               , outputFormat = CustomOutputFormat logFormat }
  scottyApp (myProcess ps logger f)

myProcess :: Pools  -> Middleware -> FilePath -> ScottyM ()
myProcess ps logger dataDir = do
  middleware logger

  middleware $ Gzip.gzip Gzip.def {Gzip.gzipFiles = Gzip.GzipCompress}

  middleware $ staticPolicy (hasPrefix "static" >-> addBase dataDir)
  get "/" $ file (dataDir ++ "/" ++ "index.html")

  get "/serverlist.json" $ json (sort $ HM.keys ps)
  get "/server/:server/processlist.json" $ apiGetProcessList ps

  -- Used by client to see which servers are really allowed by Sproxy
  addroute HEAD "/server/:server/processlist.json" $ apiCanProcessList ps

  delete "/server/:server/process/:id" $ apiKill ps

data Process = Process {
    id      :: Int
  , user    :: Text
  , host    :: Text
  , db      :: Maybe Text
  , command :: Text
  , time    :: Int
  , state   :: Maybe Text
  , info    :: Text
} deriving (Generic)
instance ToJSON Process

apiCanProcessList :: Pools -> ActionM ()
apiCanProcessList ps = do
  server <- param "server"
  case HM.lookup server ps of
    Nothing -> status notFound404 >> text server
    Just _  -> status ok200

apiKill :: Pools -> ActionM ()
apiKill ps = do
  server <- param "server"
  case HM.lookup server ps of
    Nothing -> status notFound404 >> text server
    Just p  -> do
      id <- param "id"
      if (id :: Int) == 0 then do
        [ Only f ] <- withDB p $ \c ->
                query_ c "SELECT COUNT(*) FROM information_schema.routines \
                \WHERE routine_type = 'PROCEDURE' AND routine_schema = 'mysql' \
                \AND routine_name = 'mywatch_kill'"
        if (f::Int) > 0 then status ok200
        else status notImplemented501 >> text "mywatch_kill"
      else do
        _ <- withDB p $ \c -> execute c "CALL mysql.mywatch_kill(?)" [ id ]
        status ok200

apiGetProcessList :: Pools -> ActionM ()
apiGetProcessList ps = do
  server <- param "server"
  case HM.lookup server ps of
    Nothing -> status notFound404 >> text server
    Just p  -> do
      res <- withDB p $ \c ->
              query_ c "SELECT \
              \id, user, host, db, command, time, state, info \
              \FROM information_schema.processlist \
              \WHERE info IS NOT NULL \
              \ORDER BY time DESC, id ASC"
      json $ map (\(id, user, host, db, command, time, state, info) -> Process {..}) res

withDB :: Pool Connection -> (Connection -> IO a) -> ActionM a
withDB p a = liftIO $ withResource p (liftIO . a)

