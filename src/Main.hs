{-# LANGUAGE QuasiQuotes #-}

module Main (
  main
) where

import Data.ByteString.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.Version (showVersion)
import Database.MySQL.Base (ConnectInfo(..), defaultSSLInfo)
import Database.MySQL.Base.Types (Option(ReadDefaultFile, ReadDefaultGroup))
import Paths_mywatch (getDataDir, version) -- from cabal
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import qualified Data.ConfigFile as Cf
import qualified System.Console.Docopt.NoTH as O

import Server (server)

usage :: IO String
usage = do
  dataDir <- getDataDir
  return $
    "mywatch " ++ showVersion version
    ++ " view queries on many MySQL servers" ++ [qc|

Usage:
  mywatch [options] MYCNF

Options:

  -d, --datadir=DIR        Data directory including static files [default: {dataDir}]

  -s, --socket=SOCK        Listen on this UNIX-socket [default: /tmp/mywatch.sock]
  -p, --port=PORT          Instead of UNIX-socket, listen on this TCP port (localhost)

  -h, --help               Show this message

|]

main :: IO()
main = do
  doco <- O.parseUsageOrExit =<< usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      file = fromJust $ O.getArg args $ O.argument "MYCNF"
      port = O.getArg args $ O.longOption "port"
      socket = fromJust $ O.getArg args $ O.longOption "socket"
      datadir = fromJust $ O.getArg args $ O.longOption "datadir"

    cf <- forceEither <$> Cf.readfile Cf.emptyCP file
    let
      servers = filter ("client" /=) . Cf.sections $ cf
      myInfo = map (\g -> ConnectInfo {
        connectDatabase = "",
        connectHost     = "",
        connectPassword = "",
        connectPath     = "",
        connectPort     = 0,
        -- FIXME: https://jira.mariadb.org/browse/MDEV-10246
        connectSSL      = if any (isPrefixOf "ssl") (forceEither $ Cf.options cf g)
                          then Just defaultSSLInfo else Nothing,
        connectUser     = "",
        -- FIXME: Work aroung buggy mysql: unsafeUseAsCString creates garbage.
        connectOptions  = [ ReadDefaultFile file, ReadDefaultGroup (pack $ g ++ "\0") ]
      }) servers
      listen = maybe (Right socket) (Left . read) port
    server listen myInfo datadir

