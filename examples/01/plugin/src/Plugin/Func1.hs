module Plugin.Func1 where

import API.Prelude
import API.Sub1

impl1 :: Func1
impl1 a b = pure (a > b)
