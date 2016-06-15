{-# LANGUAGE QuasiQuotes #-}

module Main (
  main
) where

import Data.ByteString.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Database.MySQL.Base (ConnectInfo(..))
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
    servers <- filter ("client" /=) . Cf.sections . forceEither <$> Cf.readfile Cf.emptyCP file
    let
      myInfo = map (\g -> ConnectInfo {
        connectDatabase = "",
        connectHost     = "",
        connectPassword = "",
        connectPath     = "",
        connectPort     = 0,
        connectSSL      = Nothing,
        connectUser     = "",
        connectOptions  = [ ReadDefaultFile file, ReadDefaultGroup (pack g) ]
      }) servers
      listen = maybe (Right socket) (Left . read) port
    server listen myInfo datadir
