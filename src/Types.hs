{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Модуль содержит типы, используемые другими модулями приложения.
module Types
  ( Board
  , GameText
  , Point

  , GameError(..)
  , GameProgress(..)
  , GameState(..)
  , Side(..)
  , Update(..)

  , newBoard
  ) where

import Data.Array (Array, listArray)
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
  deriving stock (Generic, Show)
  deriving anyclass FromForm

-- | Прогресс игры.
data GameProgress
  = InProgress           -- ^ Игра в процессе.
  | Dropped              -- ^ Игра сброшена.
  | Draw                 -- ^ Игра завершилась ничьей.
  | Win !([Point], Side) -- ^ Клетки выигравшей комбинации и сторона победителя.
  deriving stock (Eq, Show)

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
  deriving stock Show

-- | Ошибка, возникшая во время игры.
data GameError
  = CellOccupied       -- ^ Попытка походить в клетку, в которой уже был сделан ход.
  | GameNotFound       -- ^ Игра не найдена в кэше сессий.
  | TurnsDesync        -- ^ Рассинхронизация количества ходов на сервере.
  | OutOfBorder !Point -- ^ Координаты хода за пределами игрового поля.
  | ParseError !String -- ^ Ошибка парсинга хода игрока.
  deriving stock Show

instance GameText GameError where
  toGameText CellOccupied = "Так уже походили"
  toGameText (OutOfBorder point) = "Ход по координатам " <> showt point <> ", находиться за пределами поля"
  toGameText (ParseError _) = "Не получилось распарсить Update, начинаем новую игру."
  toGameText TurnsDesync = "Рассинхронизация ходов с сервером. Отправлено актуальное состояние игры."
  toGameText GameNotFound = "Ваша игра не найдена или была удалена. Начните новую игру!"

-- | Создаёт новое игровое поле.
newBoard :: Board
newBoard = listArray ((0,0), (2,2)) $ replicate 9 Nothing
