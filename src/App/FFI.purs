module App.FFI (render, displayError, setupFileUploadHandler, setGeneratedMermaid) where

import Prelude
import Effect (Effect)

foreign import render :: String -> Effect Unit

foreign import displayError :: String -> Effect Unit

foreign import setupFileUploadHandler :: (String -> Effect Unit) -> Effect Unit

foreign import setGeneratedMermaid :: String -> Effect Unit