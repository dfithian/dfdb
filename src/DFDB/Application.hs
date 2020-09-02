module DFDB.Application where

import ClassyPrelude
import Control.Monad.State (execStateT)
import System.Exit (exitSuccess)

import DFDB.Database (emptyDatabase, execute)
import DFDB.Statement (parseStatement, runParser)
import qualified DFDB.Types

greeting :: MonadIO m => m ()
greeting = putStrLn . unlines $
  [ "Welcome to DFDB"
  ]

helpText :: MonadIO m => m ()
helpText = putStrLn . unlines $
  [ "Enter \"help\" to get this text"
  , "  Quit commands: " <> intercalate ", " (DFDB.Types.unCommand <$> quitCommands)
  ]

replPrompt :: MonadIO m => m ()
replPrompt = putStr "dfdb > " >> liftIO (hFlush stdout)

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
  void . flip execStateT emptyDatabase . forever $ do
    replPrompt
    input <- DFDB.Types.Command <$> liftIO getLine
    when (input `elem` quitCommands) $ liftIO exitSuccess
    case runParser parseStatement input of
      DFDB.Types.CommandOutputFailure code -> case code of
        DFDB.Types.CommandFailureCodeParser err -> do
          putStrLn $ "Unrecognized command " <> DFDB.Types.unCommand input <> " (" <> err <> ")"
          helpText
      DFDB.Types.CommandOutputSuccess parsedStatement -> case parsedStatement of
        DFDB.Types.ParsedStatementHelp -> helpText
        DFDB.Types.ParsedStatement statement -> execute statement >>= \ case
          DFDB.Types.StatementResultSuccess output -> putStrLn $ DFDB.Types.unOutput output
          DFDB.Types.StatementResultFailure code -> case code of
            DFDB.Types.StatementFailureCodeSyntaxError err -> putStrLn err
