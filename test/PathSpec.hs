module PathSpec(main, spec) where

import Test.Hspec
import Types
import Path
import Data.List (sort)
import Data.Default
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.PSQueue as PSQ

import Control.Lens

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Path finding functionality" $ do
  let pfConf = PathFinderConfig (const True) (const 0) (const .const $ 1)
      pfState = def :: PathFinderState

  it "should be able to reconstruct the reverse path, given a predecessor map" $ do
    let pmap = Map.fromList [ (from2d (0,0), (0, Nothing))
                            , (from2d (0,1), (0, Just $ from2d (0,0)))
                            , (from2d (0,2), (0, Just $ from2d (0,1)))
                            ]
    reconstructPath (from2d (0,2)) pmap `shouldBe` map from2d [ (0,2)
                                                              , (0,1)
                                                              , (0,0)
                                                              ]

  {-
            (4,4)     (5,4)      (6,4)
                        |
                        |
                        |
            (4,5)-----(5,5)------(6,5)
                        |
                        |
                        |
            (4,6)     (5,6)      (6,6)
  -}
  it "should retrieve all possible neighbor coords" $
    (sort . neighbors) (from2d (5,5)) `shouldBe`
      sort (map from2d [ (4,4),(5,4),(6,4)
                       , (4,5),      (6,5)
                       , (4,6),(5,6),(6,6)
                       ])

  it "should not expand to disallowed coords" $ do
    let conf = pfConf & canBeWalked .~ (/= from2d (0,0))
        result = evalPathFinder conf pfState $ expand (from2d (0,1))
    from2d (0,0) `elem` result `shouldBe` False

  it "should add the cell to closed set after visiting it" $ do
    let cell = from2d (0,0)
        cellWasVisited = evalPathFinder pfConf def $ do
          visit cell
          alreadyVisited cell
    cellWasVisited `shouldBe` True

  context "analyzing neighbors" $ do
    let fromCoord = from2d (42,1337)
        toAnalyze = from2d (1,0)

    context "if the neighbor was unknown" $ do
      let pfState' = pfState & seen %~ Map.insert fromCoord (0,Just fromCoord)

      it "should insert an unknown neighbor into the predecessor map" $ do
        let result = execPathFinder pfConf pfState' $
              analyzeNbs fromCoord [toAnalyze]
            seenMap = result ^. seen
        Map.lookup (from2d (1,0)) seenMap `shouldBe` Just (1,Just fromCoord)

      it "should add new neighbors to the open set" $ do
        let result = execPathFinder pfConf pfState' $
              analyzeNbs fromCoord [toAnalyze]
            openSet = result ^. open
        isJust (PSQ.lookup toAnalyze openSet) `shouldBe` True

    it "should update the predecessor map if the new path is shorter" $ do
      let pfState' = pfState & seen %~ Map.insert toAnalyze (10,Just fromCoord)
                             & seen %~ Map.insert fromCoord (0,Just fromCoord)
          resultState = execPathFinder pfConf pfState' $ do
            visit toAnalyze
            analyzeNbs fromCoord [toAnalyze]
      Map.lookup toAnalyze (resultState ^. seen) `shouldBe` Just (1,Just fromCoord)

    it "should do nothing if the path to the predecessor is unknown" $ do
      let result = execPathFinder pfConf pfState $
                   analyzeNbs fromCoord [toAnalyze]
          seenMap = result ^. seen
      Map.lookup (from2d (1,0)) seenMap `shouldBe` Nothing

  context "finding a path" $ do

    it "should return an empty path if start == goal" $ do
      let path = evalPathFinder pfConf def $ findPath (from2d (0,0)) (from2d (0,0))
      path `shouldBe` Just []

    {-
          0   1   2
        +---+---+---+
     0  |   | S |   |
        +---+---+---+
     1  |   | . |   |
        +---+---+---+
     2  |   | G |   |
        +------------
    -}
    it "should find a straight path" $ do
      let start = from2d (1,0)
          goal = from2d (1,2)
          pfConf' = PathFinderConfig (const True) (distance goal) (const . const $ 1)
          pfState' = pfState & seen %~ Map.insert start (0,Nothing)
                             & open %~ PSQ.insert start 0
          result = evalPathFinder pfConf' pfState' $
                   findPath start goal
      result `shouldBe` (Just . map from2d $ [(1,0), (1,1),(1,2)])

    {-
         0   1   2   3   4
       +---+---+---+---+---+
     0 |   | . | S |   |   |
       +---+---+---+---+---+
     1 | . | X | X | X |   |
       +---+---+---+---+---+
     2 |   | . | G |   |   |
       +---+----------------
    -}
    it "should find a path around an obstacle" $ do
      let start = from2d (2,0)
          goal = from2d (2,2)
          blocked = map from2d [(1,1), (2,1), (3,1)]
          pfConf' = PathFinderConfig (`notElem` blocked)
                                     (distance goal)
                                     (const . const $ 1)
          pfState' = pfState & seen %~ Map.insert start (0,Nothing)
                             & open %~ PSQ.insert start 0
          result = evalPathFinder pfConf' pfState' $
                   findPath start goal
      result `shouldBe` (Just . map from2d $ [(2,0),(1,0),(0,1),(1,2),(2,2)])

    {-
          0   1   2
        +---+---+---+
     0  |   | S |   |
        +---+---+---+
     1  | x | x | x |
        +---+---+---+
     2  |   | G |   |
        +------------
    -}
    it "should terminate if path is blocked" $ do
      let start   = from2d (1,0)
          goal    = from2d (1,2)
          blocked = map from2d [(0,1), (1,1), (2,1)]
          free    = map from2d [ (x,y) | x <- [0..2], y <- [0,2] ]
          pfConf' = PathFinderConfig (\c -> c `notElem` blocked && c `elem` free)
                                     (distance goal)
                                     (const . const $ 1)
          pfState' = pfState & seen %~ Map.insert start (0,Nothing)
                             & open %~ PSQ.insert start 0
          result = evalPathFinder pfConf' pfState' $
                   findPath start goal
      result `shouldBe` Nothing

    {-
         0   1   2   3   4   5
       +---+---+---+---+---+---+
     0 |   |   | . |   | X |   |
       +---+---+---+---+---+---+
     1 |   | . | X | S | X |   |
       +---+---+---+---+---+---+
     2 |   | . | X | X | X |   |
       +---+---+---+---+---+---+
     3 |   |   | . | . | . |   |
       +---+---+---+---+---+---+
     4 | X | X | X | X | X | . |
       +---+---+---+---+---+---+
     5 | G | . | . | . | . |   |
       +---+---+---+---+---+---+
    -}
    it "should find a path around a more complicated obstacle" $ do
      let goal = from2d (0,5)
          blocked = map from2d [ (2,1) , (2,2) , (3,2)
                               , (4,0) , (4,1) , (4,2)
                               , (0,4) , (1,4) , (2,4)
                               , (3,4) , (4,4)
                               ]
          path = searchPath (\c@(Coord x y z) -> c `notElem` blocked
                                                 && z == 0
                                                 && x `elem` [0..5]
                                                 && y `elem` [0..5])
                            (distance goal)
                            (const . const $ 1)
                            (from2d (3,1))
                            (from2d (0,5))
      path `shouldBe` Just (map from2d [ (3,1)
                                       , (2,0)
                                       , (1,1)
                                       , (1,2)
                                       , (2,3), (3,3) , (4,3)
                                       , (5,4)
                                       , (4,5), (3,5), (2,5), (1,5), (0,5)
                                       ])
