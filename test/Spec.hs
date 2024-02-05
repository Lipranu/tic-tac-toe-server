module Main (main) where

import Types
import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import Data.Array ((//), listArray)
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import System.Random (randomIO)

import qualified Game

--------------------------------------------------------------------------------

main :: IO ()
main = do
  update <- mkUpdate
  state <- mkState
  hspec $ describe "Game module tests" $ do
    outOfBorderTest state update
    sameCellTest state update
    sameCountTest state update
    drawTest state update
    winTest state update

outOfBorderTest :: GameState -> Update -> Spec
outOfBorderTest state update = do
  let testRowMore = Game.run update { row = 3 } state
  it "out of border on row > 2" $ testRowMore `shouldBe` Left (OutOfBorder (0,3))
  let testRowLess = Game.run update { row = -1 } state
  it "out of border on row < 0" $ testRowLess `shouldBe` Left (OutOfBorder (0,-1))
  let testColumnMore = Game.run update { column = 3 } state
  it "out of border on column > 2" $ testColumnMore `shouldBe` Left (OutOfBorder (3,0))
  let testColumnLess = Game.run update { column = -1 } state
  it "out of border on column < 0" $ testColumnLess `shouldBe` Left (OutOfBorder (-1,0))

sameCellTest :: GameState -> Update -> Spec
sameCellTest state update = do
  let updatedState = Game.run update state
      test = Game.run update { count = 1 } <$> updatedState
  it "cell ocupied on same update" $ test `shouldBe` Right (Left CellOccupied)

sameCountTest :: GameState -> Update -> Spec
sameCountTest state update = do
  let updatedState = Game.run update state
      test = Game.run update <$> updatedState
  it "turns desynq on same update" $ test `shouldBe` Right (Left TurnsDesync)

drawTest :: GameState -> Update -> Spec
drawTest state update = do
  let test = (\s -> gameProgress s == Draw) <$> Game.run update state { board = fullBoard }
  it "draw on last cell update" $ test `shouldBe` Right True

winTest :: GameState -> Update -> Spec
winTest state update = it "win on all win combinations" test
  where
    winList = foldl' winFunction [] winCombinations
    winFunction wins comb = winGame comb : wins
    winGame [p1, p2, p3] = Game.run (winUpdate p1) $ winState p2 p3
    winState p2 p3 = state { board = board state // [(p2, Just Cross), (p3, Just Cross)] }
    winUpdate (col, row) = update { row = row, column = col }
    isWin (Right (Win _)) = True
    isWin _ = False
    test = all (isWin . fmap gameProgress) winList

mkUpdate :: IO Update
mkUpdate = do
  uuid <- randomIO
  pure $ Update { column = 0, row = 0, session = uuid, count = 0 }

mkState :: IO GameState
mkState = getCurrentTime <&> newGameState

fullBoard :: Board
fullBoard = listArray ((0,0),(2,2))
  [ Nothing   , Just Zero , Just Cross
  , Just Cross, Just Zero , Just Cross
  , Just Zero , Just Cross, Just Zero
  ]

winCombinations :: [[Point]]
winCombinations =
  [ [(0,0), (0,1), (0,2)]
  , [(1,0), (1,1), (1,2)]
  , [(2,0), (2,1), (2,2)]
  , [(0,0), (1,0), (2,0)]
  , [(0,1), (1,1), (2,1)]
  , [(0,2), (1,2), (2,2)]
  , [(0,0), (1,1), (2,2)]
  , [(2,0), (1,1), (0,2)]
  ]
