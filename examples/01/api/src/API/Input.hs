module API.Input(
    InputAPI(..)
  ) where

import Prelude

data InputAPI = InputAPI {
    _getNumber :: IO Int
  }
