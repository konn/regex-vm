{-# LANGUAGE GADTs, LiberalTypeSynonyms #-}
module Text.Regex.VM.Compile where
import Control.Alternative.Free
import Data.Functor.Coyoneda
import Text.Regex.Syntax
import Text.Regex.VM.Instructions

compile :: RE c a -> VM c a
compile = runAlt (lowerCoyoneda . hoistCoyoneda go) . unRE
  where
    go :: RE' c x -> VM c x
    go HEAD     = pos 0
    go END      = pos (- 1)
    go EPS      = pure ()
    go (SYM c)  = char c
    go (STR cs) = str cs

-- compile (SYM c)    = void $ char c
-- compile (STR cs)   = void $ str cs
-- compile (l :<>: r) = compile l *> compile r
-- compile (l :|: r)  = compile l <|> compile r
-- compile (OPT re)   = void $ optional $ compile re
-- compile (MANY re)  = void $ many $ compile re
-- compile (SOME re)  = void $ some $ compile re
