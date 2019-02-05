module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error) as EE
import Effect.Console (log, error)
import Node.ReadLine (createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (close, prompt, question, setPrompt)

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
           setPrompt "$ " interface
           dog <- question "What's your dog's name?\n" interface
           liftEffect <<< log $ "Can I pet " <> dog <> "?"
           str <- prompt interface 
           case uncons str of
             Just {head: 'y'} -> liftEffect $ log "Thanks!"
             _ -> throwError (EE.error "done")
             {-- _ -> liftEffect $ log "C'mon! Be a sport about it!" --}
