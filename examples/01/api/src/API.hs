module API(
    module API.Input
  , module API.Prelude
  , module API.Sub1
  , module API.Sub2
  , OutputAPI(..)
  ) where

import API.Input
import API.Prelude
import API.Sub1
import API.Sub2

data OutputAPI = OutputAPI {
  func1 :: Func1
, func2 :: Func2
}
