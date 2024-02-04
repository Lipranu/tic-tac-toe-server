-- | Модуль игрового процесса.
module Game (run) where

import Types

import Control.Monad.Except (MonadError, runExcept, throwError)
import Data.Array ((!), (//))
import Data.Bool (bool)
import Data.Maybe (isJust, listToMaybe, mapMaybe)

------------------------------------------------------------------------------------------

-- | Обновление игрового состояние.
run :: Update -> GameState -> Either GameError GameState
run update = runExcept . updateGame update

-- | Валидация переданного хода и обновление игрового состояния.
updateGame :: MonadError GameError m => Update -> GameState -> m GameState
updateGame Update{..} state@GameState{..} = do
  checkTurn count turnCount
  checkBounds point
  updatedBoard <- updateBoard point state
  pure $ state
    { board = updatedBoard
    , gameProgress = updateGameProgress updatedBoard
    , turnCount = count + 1
    , turnCurrent = nextTurn turnCurrent
    }
  where point = (column, row)

-- | Проверка счетчика переданного хода и ходов состояния на сервере на равенство.
checkTurn :: MonadError GameError m => Int -> Int -> m ()
checkTurn x y
  | x == y = pure ()
  | otherwise = throwError TurnsDesync

-- | Проверка хода на вхождение в границы игрового поля.
checkBounds :: MonadError GameError m => Point -> m ()
checkBounds point@(col,row)
  | check col || check row = throwError $ OutOfBorder point
  | otherwise =  pure ()
  where check x = x < 0 || x > 2

-- | Обновление игрового поля.
updateBoard :: MonadError GameError m => Point -> GameState -> m Board
updateBoard point GameState{..} = case board ! point of
    Nothing -> pure $ board // [(point, pure turnCurrent)]
    Just _  -> throwError CellOccupied

-- | Обновление состояния, проверка на завершение игры.
updateGameProgress :: Board -> GameProgress
updateGameProgress board = maybe (bool InProgress Draw draw) Win $ checkWin board
  where draw = all isJust board

-- | Поиск победителя на игровом поле.
checkWin :: Board -> Maybe ([Point], Side)
checkWin board = listToMaybe
  $ mapMaybe maybeWin
  $ mapMaybe (traverse $ maybeTuple . \p -> (p, board ! p)) winCombinations
  where
    maybeTuple :: (a, Maybe b) -> Maybe (a, b)
    maybeTuple (_, Nothing) = Nothing
    maybeTuple (p, Just s) = Just (p,s)

    maybeWin :: [(Point, Side)] -> Maybe ([Point], Side)
    maybeWin [(p1,s1),(p2,s2),(p3,s3)] = if s1 == s2 && s1 == s3 then Just ([p1,p2,p3],s1) else Nothing
    maybeWin _ = Nothing

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
