{-# LANGUAGE OverloadedStrings #-}

-- | Расширение для Control.Monad.Logger.
module Control.Monad.Logger.Extended
  ( module Control.Monad.Logger
  , logDebugNT
  , logInfoNT
  , logWarnNT
  , logErrorNT
  ) where

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)
import Data.Text.Extended (Text)
import Data.Time (getCurrentTime)

import qualified Data.Text.Extended as Text

-- | Добавляет текущие время в строку лога.
logDebugNT, logInfoNT, logWarnNT, logErrorNT :: MonadLoggerIO m => Text -> m ()
logDebugNT = logNT LevelDebug
logInfoNT  = logNT LevelInfo
logWarnNT  = logNT LevelWarn
logErrorNT = logNT LevelError

-- | Добавляет текущие время в строку лога с переданым уровнеи лога.
logNT :: MonadLoggerIO m => LogLevel -> Text -> m ()
logNT lvl t = do
  time <- liftIO getCurrentTime
  log lvl $ Text.concat ["[", Text.showt time, "] ", t]
  where
    log LevelDebug = logDebugN
    log LevelInfo  = logInfoN
    log LevelWarn  = logWarnN
    log LevelError = logErrorN
