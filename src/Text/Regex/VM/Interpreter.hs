{-# LANGUAGE DeriveGeneric, DerivingVia, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns, RankNTypes, RecordWildCards                   #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances                     #-}
module Text.Regex.VM.Interpreter where
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.RWS.Strict (RWST, evalRWST)
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxing           as U
import           GHC.Generics

import Text.Regex.Syntax
import Text.Regex.VM.Compile
import Text.Regex.VM.Instructions

newtype PC = PC Int
  deriving newtype (Show, Eq, Ord, Num, Enum)
  deriving newtype (U.Unboxable)

newtype ThreadId = ThreadId { unThreadId :: Int }
  deriving newtype (Show, Eq, Ord, Num, Enum)
  deriving newtype (U.Unboxable)

newtype Thread =
    Thread { stringPointer  :: Int }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (U.Unboxable)

newtype VMEnv c =
    VMEnv { input   :: Vector c }

newtype Machine c a =
  Machine { unMachine :: RWST (VMEnv c) () Thread (Except ThreadEvent) a }
  deriving newtype (Functor, Applicative,  Alternative, Monad, MonadPlus)
  deriving newtype (MonadError ThreadEvent)
  deriving newtype (MonadState Thread, MonadReader (VMEnv c))

instance Semigroup ThreadEvent where
  Successed <> _ = Successed
  Failed    <> m = m

instance Monoid ThreadEvent where
  mempty = Failed

instance MonadFail (Machine c) where
  fail = const $ Machine $ throwError Failed
  {-# INLINE fail #-}

data ThreadEvent = Failed | Successed
  deriving (Show, Eq, Ord, Generic)

interp0
  ::(Show c, Eq c) => Instr c a -> Machine c a
interp0 (Pos n0) = do
  len <- asks $ V.length . input
  Thread{..} <- get
  let n | n0 >= 0 = n0
        | otherwise = len + n0 + 1
  guard $ n == stringPointer
interp0 (Char c) = do
  t@Thread{..} <- get
  Just d <- asks $ (V.!? stringPointer) . input
  Just a <- pure $ c d
  put $ t { stringPointer = stringPointer + 1}
  pure a
interp0 (Str cs) = do
  t@Thread{..} <- get
  let len = V.length cs
  cs' <- asks $ V.slice stringPointer len . input
  guard $ cs == cs'
  put t { stringPointer = stringPointer + len}
  return cs

interpret
  :: (Show c, Eq c) => VM c a -> Machine c a
interpret = runVM interp0

runMachine :: Vector c -> Machine c a -> Either ThreadEvent a
runMachine input act =
  let env = VMEnv{..}
  in fmap fst $ runExcept $ evalRWST (unMachine act) env $ Thread 0

match :: (Show c, Eq c )=> RE c a -> V.Vector c -> Maybe a
match re input =
  case runMachine input $ interpret (compile re) of
    Left Failed -> Nothing
    Right a     -> Just a
    _           -> error "Result unavailable"
