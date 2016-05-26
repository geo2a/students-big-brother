module Main where

import System.Environment

import Client

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStr help
  else case args of
    [cfg] -> startClientDaemon cfg
    _          -> putStr help

-- | Help message
help :: String
help = "Students Big Brother Client v0.1.0 \n\
       \Usage: \n\
       \  students-big-brother-client <client_config.json>"
