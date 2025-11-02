module Main (main) where

import Prelude

import App.FFI (render, displayError, setupFileUploadHandler, setGeneratedMermaid)
import App.Mermaid (generateMermaid)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)

handleFileContent :: String -> Effect Unit
handleFileContent rawFile =
  case generateMermaid rawFile of
    Right generatedMermaid -> do
      log "Generated Mermaid successfully. Rendering graph..."
      setGeneratedMermaid generatedMermaid
      render generatedMermaid
    Left err -> do
      log ("Could not generate Mermaid: " <> err)
      displayError ("Error: " <> err)

main :: Effect Unit
main = do
  log "Homemade FCA started."
  setupFileUploadHandler handleFileContent
  log "Ready for file uploads."
