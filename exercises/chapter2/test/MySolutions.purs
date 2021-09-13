module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Math (sqrt, pi)

diagonal w h = sqrt $ w*w + h*h

circleArea r = pi * r*r

leftoverCents x = rem x 100
