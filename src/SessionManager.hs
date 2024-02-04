{-# LANGUAGE OverloadedStrings #-}

-- | Модуль менеджера сессий.
module SessionManager where

import Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.Extended (MonadLoggerIO, runStdoutLoggingT, logInfoNT)
import Control.Monad.Reader (MonadReader, runReaderT, ask)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import qualified Data.Text.Extended as Text
import qualified Data.HashMap.Strict as HashMap

-- | Запускает менеджер сессий.
run :: TVar Sessions -> IO ()
run = runStdoutLoggingT . runReaderT sessionManager

-- | Раз в 10 минут удаляет сессии, которые не обновлялись более 30 минут.
sessionManager :: (MonadReader (TVar Sessions) m, MonadLoggerIO m) => m ()
sessionManager = forever $ do
  liftIO $ threadDelay tenMinInMicroSec
  time <- liftIO getCurrentTime
  tvar <- ask
  info <- liftIO $ removeExpiredSessions tvar time
  logInfoNT $ logReport info
  where
    tenMinInMicroSec = 600 * 1000 * 1000
    logReport (oldCount, newCount) = Text.concat
      [ "SessionsManeger. Sessions in cache: "
      , Text.showt newCount
      , " | Removed: "
      , Text.showt $ oldCount - newCount
      ]

-- | Поиск и удаление истекших сессий.
removeExpiredSessions :: TVar Sessions -> UTCTime -> IO (Int, Int)
removeExpiredSessions tvar time = atomically $ do
  sessions <- readTVar tvar
  let filtered = HashMap.filter
        (\GameState{lastUpdate} -> time `diffUTCTime` lastUpdate < halfHourInSec)
        sessions
  writeTVar tvar filtered
  pure (HashMap.size sessions, HashMap.size filtered)
  where halfHourInSec = 1800
