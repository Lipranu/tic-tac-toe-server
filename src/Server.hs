{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | Модуль серверного приложения игры.
module Server (app) where

import Types
import Servant

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, modifyTVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.Extended (LoggingT, MonadLoggerIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.HashMap.Strict ((!?))
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import System.Random (randomIO)

import qualified Game
import qualified View
import qualified Control.Monad.Logger.Extended as Log
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Extended as Text

--------------------------------------------------------------------------------

-- | Стек монад приложения.
type AppM = ReaderT (TVar Sessions) (LoggingT Handler)

-- | API для запросов на новую игру.
type NewAPI = "new"
  :> QueryParam' '[Optional, Lenient] "session" UUID
  :> Get '[HTML] (Html ())

-- | API для запросов на обновление игры.
type UpdateAPI = "update"
  :> ReqBody' '[Required, Lenient] '[FormUrlEncoded] Update
  :> Post '[HTML] (Html ())

-- | API приложения.
type API = NewAPI :<|> UpdateAPI :<|> Raw

api :: Proxy API
api = Proxy

run :: AppM a -> TVar Sessions -> Handler a
run app = Log.runStdoutLoggingT . runReaderT app

app :: TVar Sessions -> Application
app s = serve api $ hoistServer api (`run` s) server

-- | Обработчик запросов:
--   1. new - Создание новых игровых сессий,
--   если вместе с запросом был передан идентификатор,
--   то удаляет переданную сессию из хранилища и создаёт новую
--   2. update - Обновляет игру переданным в форме ходом.
--   3. static - Отдаёт статические файлы.
server :: ServerT API AppM
server = new :<|> update :<|> static
  where
    new Nothing = do
      Log.logInfoNT "New request"
      startNewGame Nothing

    new (Just (Left error)) = do
      Log.logErrorNT $ "New request parse error: " <> Text.showt error
      startNewGame $ Just ParseError

    new (Just (Right session)) = do
      Log.logInfoNT $ "New restart request: " <> Text.showt session
      tvar <- ask
      sessions <- liftIO $ readTVarIO tvar
      let state = (\s -> s { gameProgress = Dropped }) <$> sessions !? session
      forM_ state $ updateSessionState session
      startNewGame Nothing

    update (Left error) = do
      Log.logErrorNT $ "Update request parse error: " <> Text.showt error
      startNewGame $ Just ParseError

    update (Right update@Update{..}) = do
      currentTime <- liftIO getCurrentTime
      Log.logInfoNT "Update request"
      Log.logDebugNT $ "Update content: " <> Text.showt update
      tvar <- ask
      sessions <- liftIO $ readTVarIO tvar
      case sessions !? session of
        Nothing -> do
          Log.logWarnNT $ "Session lost: " <> Text.showt session
          startNewGame $ Just GameNotFound
        Just s -> let oldState = s { lastUpdate = currentTime }
          in case Game.run update oldState of
            Left error -> do
              Log.logErrorNT $ "Game update error: " <> Text.showt error
              updateSessionState session oldState
              pure $ View.renderViewWithError error session oldState
            Right newState -> do
              Log.logDebugNT $ "Game after update: " <> Text.showt newState
              updateSessionState session newState
              pure $ View.renderView session newState

    static = serveDirectoryFileServer "static/"

-- | Обновляет сессию в хранилище, или удаляет из хранилища, если игра завершена.
updateSessionState :: UUID -> GameState -> AppM ()
updateSessionState session board = do
  tvar <- ask
  if gameEnded board
  then do
    liftIO $ atomically $ modifyTVar tvar $ HashMap.delete session
    Log.logDebugNT $ "Session ended: " <> Text.showt session
  else do
    liftIO $ atomically $ modifyTVar tvar $ HashMap.insert session board
    Log.logDebugNT $ "Session updated: " <> Text.showt session

-- | Инициализация новой игровой сессии.
startNewGame :: Maybe GameError -> AppM (Html ())
startNewGame error = do
  uuid <- liftIO randomIO
  time <- liftIO getCurrentTime
  let newGame = newGameState time
  updateSessionState uuid newGame
  Log.logDebugNT $ "Created new session: " <> Text.showt uuid
  case error of
    Nothing -> pure $ View.renderView uuid newGame
    Just error -> do
      pure $ View.renderViewWithError error uuid newGame
