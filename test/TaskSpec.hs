module TaskSpec(main, spec) where

import Test.Hspec
import Level
import Task

import Data.Ord (comparing)
import Control.Monad.State
import Control.Lens ( (^.) )
import qualified Data.Sequence as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "A Task" $ do
    let level = Level [] [] 1 S.empty S.empty

    it "can be added to a level" $ do
      let (task,level') = flip runState level $ createTask (1,1) Mine
      hasTask (task ^. taskId) level' `shouldBe` True
      comparing numberOfTasks level' level `shouldBe` GT

    it "can be assigned to a coordinate" $ do
      let (task,level') = flip runState level $ createTask (1,1) Mine
          identifier = task ^. taskId
      (fst . getTask identifier $ level') `shouldBe` (1,1)