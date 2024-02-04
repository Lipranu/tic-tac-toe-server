{-# LANGUAGE OverloadedStrings #-}

-- | Модуль отрисовки игры в HTML.
module View (renderView, renderViewWithError) where

import Types
import Lucid.Html5

import Control.Monad (forM_)
import Data.Array ((!), assocs)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text.Extended (showt, empty)
import Data.UUID (UUID, toText)
import Lucid (Html, toHtml)

------------------------------------------------------------------------------------------

-- | Отрисовка интерфейса с выводом ошибки.
renderViewWithError :: GameError -> UUID -> GameState -> Html ()
renderViewWithError error = renderViewInternal (Just error)

-- | Отрисовка пользовательского интерфейса без ошибок.
renderView :: UUID -> GameState -> Html ()
renderView = renderViewInternal Nothing

-- | Отрисовка пользовательского интерфейса. Для использования внутри модуля.
renderViewInternal :: Maybe GameError -> UUID -> GameState -> Html ()
renderViewInternal error session gs@GameState{..} = doctypehtml_ $ html_ $ do
  renderHead
  body_ $ do
    renderTitle gs
    renderBoard gs session
    renderNewGameButton gs session
    renderError error

-- | Шаблон головного эллемента HTML.
renderHead :: Html ()
renderHead = head_ $ do
  title_ "Tic-Tac_Toe"
  link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]
  link_ [rel_ "stylesheet", type_ "text/css", href_ "css/normalize.css"]
  meta_ [content_ "text/html; charset=UTF-8", httpEquiv_ "Content-Type"]

-- | Заголовок игры.
renderTitle :: GameState -> Html ()
renderTitle GameState{..} = h1_ [class_ "title"] $ case (gameProgress, turnCurrent) of
  (InProgress, Cross) -> "Сейчас ходит: " <> span_ [class_ "cross"] "X"
  (InProgress, Zero) -> "Сейчас ходит: " <> span_ [class_ "zero"] "O"
  (Win (_, Cross), _) -> "Победитель: " <> span_ [class_ "cross"] "X"
  (Win (_, Zero), _) -> "Победитель: " <> span_ [class_ "zero"] "O"
  (Draw, _) -> "Ничья"
  (Dropped, _) -> "Игра прервана"

-- | Поле с выводом ошибок.
renderError :: Maybe GameError -> Html ()
renderError mError = div_ [class_ "bar"] $ case mError of
  Just error -> p_ [class_ "error"] $ toHtml $ toGameText error
  Nothing -> toHtml empty

-- | Кнопка новой игры.
renderNewGameButton :: GameState -> UUID -> Html ()
renderNewGameButton gs session = div_ [class_ "bar"] $ if gameEnded gs
  then a_ [class_ "newgame", href_ "/new"] "Новая игра"
  else a_ [class_ "newgame", href_ $ "/new?session=" <> showt session] "Начать заново"

-- | Отрисовка игрового поля.
renderBoard :: GameState -> UUID -> Html ()
renderBoard gs@GameState{board} session = div_ [class_ "board"]
  $ forM_ (assocs board)
  $ renderCell gs session

-- | Отрисовка клетки поля.
renderCell :: GameState -> UUID -> (Point, Maybe Side) -> Html ()
renderCell gs@GameState{..} session (cell@(col, row), mSide) = form_ [method_ "post", action_ "/update"] $ do
  input_ [type_ "text", name_ "session", hidden_ mempty, value_ $ toText session]
  input_ [type_ "number", name_ "column", hidden_ mempty, value_ $ showt col]
  input_ [type_ "number", name_ "row", hidden_ mempty, value_ $ showt row]
  input_ [type_ "number", name_ "count", hidden_ mempty, value_ $ showt turnCount]
  input_ $ [type_ "submit", class_ "cell", value_ $ toGameText mSide]
         & disableMarked
         & renderColor
         & renderWinCell
  where
    disableMarked attrs =
      if gameEnded gs || isJust mSide
      then disabled_ mempty : attrs
      else attrs

    renderColor attrs = case mSide of
      Nothing    -> value_ (toGameText turnCurrent) : attrs
      Just Cross -> class_ " cross" : attrs
      Just Zero  -> class_ " zero" : attrs

    renderWinCell attrs = case gameProgress of
      Win (winCells, _) ->
        if cell `elem` winCells
        then class_ " win" : attrs
        else attrs
      r -> attrs
