{-# LANGUAGE DerivingVia, GADTs, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances      #-}
{-# OPTIONS_GHC -Wall #-}
module Text.Regex.VM.Instructions where
import           Control.Alternative.Free
import           Control.Applicative
import           Control.Monad.Except
import           Data.Functor.Coyoneda
import qualified Data.Vector              as V

data Instr c a where
  Char  :: !(c -> Maybe a) -> Instr c a
  Str   :: V.Vector c -> Instr c (V.Vector c)
  Pos   :: !Int -> Instr c ()

type VM c = Alt (Coyoneda (Instr c))

char :: (c -> Maybe a) -> VM c a
char = liftAlt . liftCoyoneda . Char
{-# INLINE char #-}

str :: V.Vector c -> VM c (V.Vector c)
str = liftAlt . liftCoyoneda . Str

pos :: Int -> VM c ()
pos = liftAlt . liftCoyoneda . Pos

runVMF
  :: forall f m a. Alternative m
  => (forall x. f x -> m x)
  -> Alt (Coyoneda f) a -> m a
runVMF handle = runAlt (lowerCoyoneda . hoistCoyoneda handle)

runVM
  :: forall f m a. MonadPlus m
  => (forall x. f x -> m x)
  -> Alt (Coyoneda f) a -> m a
runVM handle = runAlt (lowerM . hoistCoyoneda handle)
