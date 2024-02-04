-- | Модуль расширение для Data.Text.Extended.
module Data.Text.Extended
  ( module Data.Text
  , showt
  ) where

import Data.Text

-- | Преобразует в текст типы, реализующие класс Show.
showt :: Show a => a -> Text
showt = pack . show
