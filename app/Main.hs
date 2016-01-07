module Main where

import System.Environment

import Server
import Client

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 
  then putStr help
  else case args of
    ["--client", cfg] -> startClientDaemon cfg
    ["--server", cfg] -> startServer cfg
    _          -> putStr help

-- | Help message
help :: String
help = "Students Big Brother v0.1 \n\
       \Usage: \n\
       \  --server <server_config.json>   run in server mode \n\
       \  --client <client_config.json>   run in client daemon mode \n"
