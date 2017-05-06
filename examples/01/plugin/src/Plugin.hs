module Plugin where

import API
import Plugin.Func1
import Plugin.Func2

plugin :: API
plugin = API {
    func1 = impl1
  , func2 = impl2
  }
