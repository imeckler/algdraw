module Draw where


import           Alg
import           Utils
import qualified Data.Vector as V
import           Data.Monoid
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Gloss.Data.Vector

arrow :: Point -> Point -> Picture
arrow (xf, yf) (xt, yt) = translate xf yf $ rotate (radiansToDegrees $ angleVV (0, 1) dir) arr
  where
    triangle = let bottomY = len - sideLen * (sqrt 3 / 2)
               in  polygon [(0, len), (sideLen / 2, bottomY), (- sideLen / 2, bottomY)]
    arr     = line [(0,0), (0, len)] <> triangle
    sideLen = 10
    dir     = ((xt - xf), (yt - yf))
    len     = magV dir

    radiansToDegrees = (* (180 / pi))

drawPerm :: Perm -> Picture
drawPerm p = circs <> translate sep 0 circs <> arrows
  where
    circs = pictures . take n
          $ zipWith (translate 0) (iterate (subtract spacing) 0)
                                  (repeat (circle radius & color black))

    arrows = pictures . V.toList $ V.imap (\i k -> arrow (0, centerY i) (sep, centerY k)) p
    windowHeight = 500
    n            = V.length p
    spacing      = windowHeight / fromIntegral n
    radius       = 0.8 * spacing / 2
    sep          = 60
    centerY i    = - spacing * fromIntegral i

