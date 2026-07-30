{-|
Module      : Quote.Base
Description : Base file for QuasiQuoter.
Copyright   : (c) Guilherme Drummond, Rodrigo Ribeiro, 2025
License     : BSD-3-Clause
Maintainer  : rodrigo.ribeiro@ufop.edu.br
Stability   : experimental
Portability : POSIX

This module provides basic functions for QuasiQuoters.
-}
module Quote.Base 
    ( topLevel
    , parseIO
    , location'
    , setPosition
    ) where

import Parser.Base (Parser, sc)
import Text.Megaparsec
    ( parse
    , MonadParsec(eof, updateParserState)
    , SourcePos(..)
    , mkPos
    , PosState (pstateSourcePos)
    , State (statePosState), errorBundlePretty
    )
import Language.Haskell.TH
    (Q, location, Loc(loc_start, loc_filename))
import Control.Exception (throwIO)

{-|
Parses a parser from the beginning of input and requires that the parser
consumes all remaining whitespace and reaches end of file.
-}
topLevel :: Parser a -> Parser a
topLevel p = sc *> p <* eof

{-|
Parse a string using the provided parser and raise an IO exception on parse
failure.
-}
parseIO :: Parser a -> String -> IO a
parseIO p str =
    case parse p "" str of
        Left err -> throwIO (userError (errorBundlePretty err))
        Right a  -> return a

{-|
Return the current Template Haskell source position as a Megaparsec
position, t'Text.Megaparsec.SourcePos'.
-}
location' :: Q SourcePos
location' = aux <$> location
    where
        aux :: Loc -> SourcePos
        aux loc =
            let (line, col) = loc_start loc
            in SourcePos 
            {   sourceName = loc_filename loc
            ,   sourceLine = mkPos line
            ,   sourceColumn = mkPos col 
            }

{-|
Set the parser state position to the given t'Text.Megaparsec.SourcePos'.
-}
setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \state ->
    let pst = statePosState state
        pst' = pst { pstateSourcePos = pos }
    in state { statePosState = pst' }
