module Kube where




import Node.ReadLine.Aff

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, throwError)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecOptions, exec)
import Node.Encoding (Encoding(..))
import Prelude (Unit, discard, pure, ($), (*>), (<<<), (<>), (=<<))

configHandle :: Interface -> Aff String 
configHandle interface = question "Enter kubernetes config path" interface
    {-- (liftEffect $ exec ("export KUBECONFIG = " <> configPath) defaultExecOptions (\x -> pure unit)) >>= (liftEffect <<< kill SIGKILL) --}

exitHandle :: Interface -> Effect Unit
exitHandle interface = do
    log "Exiting, Thanks and Bye"
    close interface

namespaceHandle :: Interface -> Aff String 
namespaceHandle = question "Enter the namespace"

showHelp :: Interface -> Aff String 
showHelp interface = do
    setPrompt """ Enter CC any time on prompt to change the Kube Config
                  GN - Get nodes
                  GP - Get Pods
                  CN - Change Namespace
 """ interface
    prompt interface



executeCommand :: String -> String -> String -> Aff String
executeCommand "GN" ns env = (\out -> liftEffect $ toString UTF8 out.stdout) =<< (makeAff (\cb -> exec ("export KUBECONFIG=" <> env <>" && kubectl get nodes -n " <> ns) defaultExecOptions (cb <<< Right) *> (pure nonCanceler)))
executeCommand "GP" ns env = (\out -> liftEffect $ toString UTF8 out.stdout) =<< (makeAff (\cb -> exec ("export KUBECONFIG=" <> env <>" && kubectl get pods -n " <> ns) defaultExecOptions (cb <<< Right) *> (pure nonCanceler)))
executeCommand "CN" ns _ = throwError (error "Change Name Space")
executeCommand "CC" ns _ = throwError (error "Change Kube Config")
executeCommand _ ns _ = throwError (error "Unknow command")

