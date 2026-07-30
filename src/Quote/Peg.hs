{-|
Module      : Quote.Peg
Description : QuasiQuoter for PEGs (Parsing Expression Grammars).
Copyright   : (c) Guilherme Drummond, 2025
License     : MIT
Maintainer  : guiadnguto@gmail.com
Stability   : experimental
Portability : POSIX

This module provides QuasiQuoters for PEGs (Parsing Expression Grammars),
including definitions, expressions, and PEG operators.
-}
module Quote.Peg 
    ( grammar
    ) where

import Quote.Base ( topLevel, parseIO, location', setPosition )
import qualified Parser.Peg as Peg
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Quote (dataToExpQ, QuasiQuoter(..))

{-|
QuasiQuoter for PEG grammar syntax.

Parses a grammar string at compile time and converts it into a Template
Haskell expression.
-}
grammar :: QuasiQuoter
grammar = QuasiQuoter {
        quoteExp = \ str -> do
            l <- location'
            c <- runIO $ parseIO (setPosition l *> topLevel Peg.grammar) str
            dataToExpQ (const Nothing) c
        , quotePat  = undefined
        , quoteType = undefined
        , quoteDec  = undefined
    }