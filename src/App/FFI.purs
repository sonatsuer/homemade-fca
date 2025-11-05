module App.FFI (renderAndDisplayCode, displayError, setupFileReadHandler) where

import Prelude
import Effect (Effect)

foreign import renderAndDisplayCode :: String -> Effect Unit

foreign import displayError :: String -> Effect Unit

foreign import setupFileReadHandler :: (String -> Effect Unit) -> Effect Unit
