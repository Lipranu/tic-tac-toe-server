{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Модуль содержит типы, используемые другими модулями приложения.
module Types
  ( Board
  , Point
  , Sessions

  , GameError(..)
  , GameProgress(..)
  , GameState(..)
  , GameText(..)
  , Side(..)
  , Update(..)

  , gameEnded
  , newBoard
  , newGameState
  , nextTurn
  ) where

import Data.Array (Array, listArray)
import Data.HashMap.Strict (HashMap)
import Data.Text.Extended (Text, showt)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------

-- | Клетка игрового поля.
type Point = (Int, Int)

-- | Игровое поле.
type Board = Array Point (Maybe Side)

-- | Коллекция активныхигровых сессий.
type Sessions = HashMap UUID GameState

-- | Класс преобразует данные в текст для вывода игроку.
class GameText a where
  toGameText :: a -> Text

instance GameText a => GameText (Maybe a) where
  toGameText :: GameText a => Maybe a -> Text
  toGameText (Just x) = toGameText x
  toGameText Nothing = mempty

-- | Тип, содержащий данные хода игрока.
data Update = Update
  { session :: !UUID -- ^ Идентификатор игровой сессии.
  , row     :: !Int  -- ^ Индекс ряда хода игрока.
  , column  :: !Int  -- ^ Индекс колонки хода игрока.
  , count   :: !Int  -- ^ Счетчик ходов.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass FromForm

-- | Прогресс игры.
data GameProgress
  = InProgress           -- ^ Игра в процессе.
  | Dropped              -- ^ Игра сброшена.
  | Draw                 -- ^ Игра завершилась ничьей.
  | Win !([Point], Side) -- ^ Клетки выигравшей комбинации и сторона победителя.
  deriving stock (Show, Eq)

-- | Сторона игрока.
data Side
  = Cross -- ^ Крестики.
  | Zero  -- ^ Нолики.
  deriving stock (Show, Eq)

instance GameText Side where
  toGameText :: Side -> Text
  toGameText Cross = "X"
  toGameText Zero  = "O"

-- | Состояние игровой сессии.
data GameState = GameState
  { board        :: !Board        -- ^ Состояние игрового поля.
  , gameProgress :: !GameProgress -- ^ Прогресс игры.
  , lastUpdate   :: !UTCTime      -- ^ Время последней игровой активности.
  , turnCount    :: !Int          -- ^ Счетчик ходов.
  , turnCurrent  :: !Side         -- ^ Сторона для текущего хода.
  }
  deriving stock (Show, Eq)

-- | Ошибка, возникшая во время игры.
data GameError
  = CellOccupied       -- ^ Попытка походить в клетку, в которой уже был сделан ход.
  | GameNotFound       -- ^ Игра не найдена в кэше сессий.
  | TurnsDesync        -- ^ Рассинхронизация количества ходов на сервере.
  | ParseError         -- ^ Ошибка парсинга хода игрока.
  | OutOfBorder !Point -- ^ Координаты хода за пределами игрового поля.
  deriving stock (Show, Eq)

instance GameText GameError where
  toGameText :: GameError -> Text
  toGameText CellOccupied = "Клетка уже занята."
  toGameText (OutOfBorder point) = "Ход по координатам " <> showt point <> ", находиться за пределами поля"
  toGameText ParseError = "Не получилось прочитать форму запроса. Начата новая игра."
  toGameText TurnsDesync = "Рассинхронизация ходов с сервером. Отправлено актуальное состояние игры."
  toGameText GameNotFound = "Ваша игра не найдена или была удалена. Начата новая игра."

-- | Создаёт новое игровое поле.
newBoard :: Board
newBoard = listArray ((0,0), (2,2)) $ replicate 9 Nothing

-- | Проверка текущего состояние игры на завершение.
gameEnded :: GameState -> Bool
gameEnded GameState{gameProgress} = gameProgress /= InProgress

-- | Возвращает следующих ход.
nextTurn :: Side -> Side
nextTurn Cross = Zero
nextTurn Zero = Cross

-- | Создание нового игрового состояния.
newGameState :: UTCTime -> GameState
newGameState lastUpdate = GameState
  { gameProgress = InProgress
  , board = newBoard
  , turnCount = 0
  , turnCurrent = Cross
  , lastUpdate
  }
