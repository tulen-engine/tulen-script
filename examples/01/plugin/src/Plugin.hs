module Plugin where

import API

plugin :: API
plugin = API {
    func1 = \a b -> pure (a > b)
  , func2 = \a b -> pure (a < b)
  }
