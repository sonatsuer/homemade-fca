module App.FFI (render, displayError, setupFileUploadHandler) where

import Prelude
import Effect (Effect)

foreign import render :: String -> Effect Unit

foreign import displayError :: String -> Effect Unit

foreign import setupFileUploadHandler :: (String -> Effect Unit) -> Effect Unit
