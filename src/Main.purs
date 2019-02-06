module Main where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Foreign.Object (empty, insert)
import Kube (configHandle, executeCommand, namespaceHandle, showHelp)
import Node.ReadLine (createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (close)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  runAff_ (either 
              (\err -> showError "throwin" *> close interface) 
              (const $ close interface)) 
           (loop interface)
    where
       showError err = error (show err)
       loop interface = do
           config <- configHandle interface
           ns <- namespaceHandle interface
           input <- showHelp interface
           out <- executeCommand input ns config 
           liftEffect $ log out
