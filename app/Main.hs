module Main where

import System.Environment

import Server
import Client

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 
  then print help
  else case args of
    ["--client", cfg] -> startClientDaemon cfg
    ["--server", cfg] -> startServer cfg
    _          -> print help

help :: String
help = "help"
