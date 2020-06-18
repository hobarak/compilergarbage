{-# LANGUAGE Strict #-}

module Lib where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Strict
import Data.Maybe (fromJust)
import Data.Semigroup (Max (Max), getMax)
