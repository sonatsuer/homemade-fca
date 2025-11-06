module Main (main) where

import Prelude

import App.FFI (renderAndDisplayCode, displayError, setupFileReadHandler)
import App.Mermaid (generateMermaid)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)

handleFileContent :: String -> Effect Unit
handleFileContent rawFile =
  case generateMermaid rawFile of
    Right generatedMermaid -> do
      log "Generated Mermaid successfully. Rendering graph..."
      renderAndDisplayCode generatedMermaid
    Left err -> do
      log ("Could not generate Mermaid: " <> err)
      displayError ("Error: " <> err)

main :: Effect Unit
main = do
  log "Homemade FCA started."
  setupFileReadHandler handleFileContent
  log "Ready to read CSVs."
