import Criterion.Config
import Criterion.Main

import Data.Monoid

import Path
import Coords

config = defaultConfig {
  cfgReport = Last $ Just "dist/a-star.html"
}

main :: IO ()
main = defaultMainWith
  config
  (return ())
  [ bench "Straight path in open space" $ nf (twoDimSearchOpen (0,0)) (5000,5000)
  , bench "Around a wall" $
      nf (twoDimSearch simpleWall (1,0)) (-1,0)
  ]

twoDimSearch :: (Coord -> Bool) -> (Int,Int) -> (Int,Int) -> Maybe Path
twoDimSearch p start goal = searchPath p heur (const . const $ 1) (from2d start) (from2d goal)
  where heur = simpleDistance (from2d goal)

simpleDistance (Coord x y z) (Coord x2 y2 z2) = fromIntegral $ sum . map abs $ [x-x2,y-y2,z-z2]

twoDimSearchOpen :: (Int,Int) -> (Int,Int) -> Maybe Path
twoDimSearchOpen = twoDimSearch (const True)

simpleWall :: Coord -> Bool
simpleWall (Coord x y z) = z == 0
                        && not ( x == 0
                              && y `elem` [-75..75])
