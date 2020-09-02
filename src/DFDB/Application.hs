module DFDB.Application where

import ClassyPrelude
import System.Exit (exitSuccess)

import DFDB.Statement (parseStatement, runParser)
import qualified DFDB.Types

greeting :: IO ()
greeting = putStrLn . unlines $
  [ "Welcome to DFDB"
  ]

helpText :: IO ()
helpText = putStrLn . unlines $
  [ "Enter \"help\" to get this text"
  , "  Quit commands: " <> intercalate ", " (DFDB.Types.unCommand <$> quitCommands)
  ]

replPrompt :: IO ()
replPrompt = putStr "dfdb > " >> hFlush stdout

quitCommands :: [DFDB.Types.Command]
quitCommands =
  [ DFDB.Types.Command ":q"
  , DFDB.Types.Command "quit()"
  , DFDB.Types.Command "exit"
  ]

dfdbRepl :: IO ()
dfdbRepl = do
  greeting
  helpText
  forever $ do
    replPrompt
    input <- DFDB.Types.Command <$> getLine
    when (input `elem` quitCommands) exitSuccess
    case runParser parseStatement input of
      DFDB.Types.CommandOutputFailure code -> case code of
        DFDB.Types.FailureCodeParser err -> do
          putStrLn $ "Unrecognized command " <> DFDB.Types.unCommand input <> " (" <> err <> ")"
          helpText
      DFDB.Types.CommandOutputSuccess statement -> case statement of
        DFDB.Types.StatementHelp -> helpText
        other -> putStrLn $ "Got statement " <> tshow other
