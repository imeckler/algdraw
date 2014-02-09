{-# LANGUAGE   TypeOperators
             , DataKinds #-}

import Alg
import Data.Modular
import Draw
import Graphics.Gloss.Interface.Pure.Display

g :: Group (Int/3, Int/3)
g = prod zModN zModN

main :: IO ()
main = display (InWindow "hi" (1,1) (200, 200)) white (drawPerm (reify g (1, 2)))

