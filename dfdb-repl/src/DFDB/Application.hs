module DFDB.Application where

import ClassyPrelude
import System.Exit (exitSuccess)

newtype Command = Command { unCommand :: Text }
  deriving (Eq, Ord, Show)

greeting :: IO ()
greeting = putStrLn . unlines $
  [ "Welcome to DFDB"
  ]

replPrompt :: IO ()
replPrompt = putStr "dfdb > " >> hFlush stdout

quitCommands :: [Command]
quitCommands =
  [ Command ":q"
  , Command "quit()"
  , Command "exit"
  ]

dfdbRepl :: IO ()
dfdbRepl = do
  greeting
  forever $ do
    replPrompt
    input <- Command <$> getLine
    when (input `elem` quitCommands) exitSuccess
    putStrLn . unCommand $ input
