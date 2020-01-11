{-# LANGUAGE DerivingStrategies, FlexibleContexts, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                  #-}
module Text.Regex.Syntax where
import           Control.Alternative.Free
import           Control.Applicative
import           Control.Monad
import           Data.Functor.Coyoneda
import           Data.MonoTraversable
import qualified Data.Vector              as V

data RE' c a where
  HEAD :: RE' c ()
  END  :: RE' c ()
  EPS  :: RE' c ()
  SYM  :: (c -> Maybe a) -> RE' c a
  STR  :: V.Vector c -> RE' c (V.Vector c)

newtype RE c a = RE { unRE :: Alt (Coyoneda (RE' c)) a }
  deriving newtype (Functor, Applicative, Alternative)

start :: RE c ()
start = RE $ liftAlt $ liftCoyoneda HEAD

endOfInput :: RE c ()
endOfInput = RE $ liftAlt $ liftCoyoneda END

psym :: (c -> Maybe a) -> RE c a
psym = RE . liftAlt . liftCoyoneda . SYM

sym :: Eq a => a -> RE a a
sym c = RE $ liftAlt $ liftCoyoneda $ SYM $ \d -> d <$ guard (d == c)

string
  :: (Eq (Element mono), MonoFoldable mono)
  => mono -> RE (Element mono) mono
string i = i <$ RE (liftAlt . liftCoyoneda . STR . V.fromList . otoList $ i)
