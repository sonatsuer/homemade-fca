module Main where

import Prelude
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Console (log)

import App.FFI (render, displayError, setupFileUploadHandler)

preprocess :: String -> Either String String
preprocess = Right

-- | 2. The Impure Handler Function (Effect)
-- | This function receives the raw file content from the FFI.
handleFileContent :: String -> Effect Unit
handleFileContent rawContent =
  case preprocess rawContent of
    Right diagramSource -> do
      log "Preprocessing successful. Rendering graph..."
      render diagramSource
    Left err -> do
      log ("Preprocessing failed: " <> err)
      displayError ("Error: " <> err)


-- | 3. Main program setup.
main :: Effect Unit
main = do
  log "PureScript application started."
  log "Setting up file upload handler with PureScript preprocessor."
  -- Pass the impure handler function to the FFI setup handler.
  setupFileUploadHandler handleFileContent
  log "Ready for file uploads."