{-|
Module      : Quote.Pattern
Description : QuasiQuoter for patterns.
Copyright   : (c) Guilherme Drummond, Rodrigo Ribeiro, 2025
License     : BSD-3-Clause
Maintainer  : rodrigo.ribeiro@ufop.edu.br
Stability   : experimental
Portability : POSIX

This module provides QuasiQuoters for patterns, including definitions, 
expressions, and PEG operators.
-}
module Quote.Pattern 
    ( patterns
    ) where

import Quote.Base ( topLevel, parseIO, location', setPosition )
import qualified Parser.Pattern as Pattern
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Quote (dataToExpQ, QuasiQuoter(..))

{-|
QuasiQuoter for pattern syntax.

Parses a pattern string at compile time and converts it into a Template
Haskell expression.
-}
patterns :: QuasiQuoter
patterns = QuasiQuoter {
        quoteExp = \ str -> do
            l <- location'
            c <- runIO $ parseIO (setPosition l *> topLevel Pattern.patterns) str
            dataToExpQ (const Nothing) c
        , quotePat  = undefined
        , quoteType = undefined
        , quoteDec  = undefined
    }