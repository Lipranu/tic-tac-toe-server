module Main where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.HashMap.Strict (empty)
import Network.Wai.Handler.Warp (run)

import qualified Server
import qualified SessionManager

--------------------------------------------------------------------------------

-- | Запускает приложение и менеджер сессий.
main :: IO ()
main = do
  initialState <- newTVarIO empty
  let runApp = run port $ Server.app initialState
      runManager = SessionManager.run initialState
  concurrently_ runApp runManager
  where port = 8080
