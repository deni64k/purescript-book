module Main where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.RWS.Trans (RWSResult(..), runRWST)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameState (GameState, initialGameState)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Console (log)
import Game (game)
import Node.ReadLine as RL
import Node.Yargs.Applicative (Y, runY, flag, yarg)
import Node.Yargs.Setup (usage)

runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      case unwrap $ runExceptT $ runRWST (game (split (wrap " ") input)) env currentState of
        Right (RWSResult state _ written) -> do
          for_ written log
          RL.setLineHandler (lineHandler state) $ interface
        Left err -> do
          log $ "Error occurred: " <> err
      RL.prompt interface
      pure unit

  RL.setLineHandler (lineHandler initialGameState) interface
  RL.prompt interface

  pure unit

main :: Effect Unit
main = runY (usage "$0 -p <player name>") $ map runGame env
  where
  env :: Y GameEnvironment
  env = gameEnvironment
          <$> yarg "p" ["player"]
                       (Just "Player name")
                       (Right "The player name is required")
                       false
          <*> flag "d" ["debug"]
                       (Just "Use debug mode")
          <*> flag "c" ["cheat"]
                       (Just "Use cheat mode")
