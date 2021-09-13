module Euler where

import Prelude
import Data.List (range, filter)
import Data.Foldable (sum)

ns n = range 0 (n - 1)

multiples n = filter f $ ns n
              where f n = mod n 3 == 0 || mod n 5 == 0

answer n = sum $ multiples n
