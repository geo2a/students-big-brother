module Main where

import System.Environment

import Server

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStr help
  else case args of
    [cfg] -> startServer cfg
    _          -> putStr help

-- | Help message
help :: String
help = "Students Big Brother Server v0.1.0 \n\
       \Usage: \n\
       \  students-big-brother-server <server_config.json>\n"
