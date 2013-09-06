module TaskSpec(main, spec) where

import Test.Hspec
import Level
import Level.Task
import Task
import Coords

import Data.Ord (comparing)
import Control.Monad.State
import Control.Lens ( (^.) )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Task" $ do
    let level = emptyLevel
        coord = Coord 1 1 1

    it "can be added to a level" $ do
      let (task,level') = flip runState level $ createTask coord Mine
      hasTask (task ^. taskId) level' `shouldBe` True
      comparing numberOfTasks level' level `shouldBe` GT

    it "can be assigned to a coordinate" $ do
      let (task,level') = flip runState level $ createTask coord Mine
          identifier = task ^. taskId
      (fst `fmap` getTask identifier level') `shouldBe` Just coord
