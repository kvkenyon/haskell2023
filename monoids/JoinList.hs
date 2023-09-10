{-# LANGUAGE GADTs #-}

module JoinList where
import           Text.XHtml (lang)

data JoinList m a where
  Empty   :: JoinList m a
  Single  :: m -> a -> JoinList m a
  Append  :: m -> JoinList m a -> JoinList m a -> JoinList m a
  deriving (Eq, Show)
