{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

{-|
Module      : Match.Capture
Description : Functions for matching and capturing patterns in syntax trees.
Copyright   : (c) Guilherme Drummond, Rodrigo Ribeiro, 2025
License     : BSD-3-Clause
Maintainer  : rodrigo.ribeiro@ufop.edu.br
Stability   : experimental
Portability : POSIX

This module provides functions to check pattern matching ('Pattern') in
AST ('ParsedTree') and capture corresponding subtrees.
-}
module Match.Capture
    ( match
    , match'
    , capture
    ) where

import Syntax.Pattern (Pattern(..))
import Syntax.ParsedTree (ParsedTree(..), ParsedTreeZipper, goDown, goLeft, goRight, goUp, pullFromRight, ofExpression)
import Data.Generics (mkQ, everything)
import Syntax.Peg (Grammar)
import Data.Maybe (isJust)

{-|
Checks if a pattern ('Pattern') matches an AST ('ParsedTree').

The 'match'' function traverses the tree and checks if the structure and values match the given pattern.
Matching is anchored at the focus of the zipper: it does not search the subtrees.
On success it returns the bindings produced by the variables ('PatVar') of the pattern.

The grammar is consulted only to resolve the expression of a 'PatVar', via 'ofExpression'.

=== Usage examples:

>>> let g = ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")

>>> match' g (PatT (T "a")) (ParsedT (T "a"), [])
Just []

>>> match' g (PatT (T "a")) (ParsedT (T "b"), [])
Nothing

@since 1.0.0
-}
match' :: Grammar -> Pattern -> ParsedTreeZipper -> Maybe [(Pattern, ParsedTree)]
match' g p@(PatVar e _)         z@(t, _) =
    if ofExpression g e t
        then Just [(p, t)]
        else match' g p =<< e2
    where
        up = goUp z
        e1 = (\(expr, z') -> (,z') <$> pullFromRight expr) =<< up
        e2 = goLeft =<< e1
match' _ PatEpsilon           (ParsedEpsilon, _)         = 
    Just []
match' _ (PatNot _)           (ParsedNot, _)             = 
    Just []
match' g (PatNT nt p)         z@(ParsedNT nt' _, _)      =
    if nt == nt'
        then match' g p =<< goDown z
        else Nothing
match' _ (PatT t)             (ParsedT t', _)          = 
    if t == t' then Just [] else Nothing
match' g (PatSeq p1 p2)       z@(ParsedSeq _ _, _)   = 
    (++) <$> (match' g p1 =<< goLeft z) <*> (match' g p2 =<< goRight z)
match' g (PatSeq p1 p2)       z@(ParsedIndent _ _, _) = 
    (++) <$> (match' g p1 =<< goLeft z) <*> (match' g p2 =<< goRight z) 
match' g (PatChoice p1 _)     z@(ParsedChoiceLeft _, _)  = 
    match' g p1 =<< goDown z
match' g (PatChoice _ p2)     z@(ParsedChoiceRight _, _) = 
    match' g p2 =<< goDown z
match' g (PatStar p)          (ParsedStar ts, _)         = 
    foldr (\ x xs -> (++) <$> match' g p (x, []) <*> xs) (Just []) ts
match' g (PatStarSeq ps)      (ParsedStar ts, _)         = 
    if length ps == length ts
    then foldr (\ x y -> (++) <$> x <*> y) (Just []) $ zipWith (\ x y -> match' g x (y, [])) ps ts
    else Nothing
match' _ _                    _                          = 
    Nothing

{-|
Checks if a pattern ('Pattern') matches any subtree of an AST ('ParsedTree').

Unlike 'match'', which is anchored at the root, this function applies 'match'' to
every subtree and succeeds if any of them matches.

=== Usage examples:

>>> let g = ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")

>>> match g (PatT (T "a")) (ParsedT (T "a"))
True

The pattern needs to match only a subtree, not the whole tree:

>>> match g (PatT (T "a")) (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b")))
True

@since 1.0.0
-}
match :: Grammar -> Pattern -> ParsedTree -> Bool
match g p = everything (||) (False `mkQ` (isJust . match' g p . (, [])))

{-|
Captures all subtrees of an AST ('ParsedTree') that match a variable ('PatVar').

The 'capture' function applies 'match'' to every subtree and concatenates the
bindings of the matches it finds.

=== Usage examples:

>>> let g = ([(NT "S", Sequence (ExprT (T "a")) (ExprT (T "b")))], NT "S")
>>> let tree = ParsedSeq (ParsedT (T "a")) (ParsedT (T "b"))

>>> capture g (PatSeq (PatT (T "a")) (PatVar (ExprT (T "b")) "B")) tree
[[(PatVar (ExprT (T "b")) "B",ParsedT (T "b"))]]

>>> capture g (PatVar (ExprT (T "a")) "A") tree
[[(PatVar (ExprT (T "a")) "A",ParsedT (T "a"))]]

@since 1.0.0
-}
capture :: Grammar -> Pattern -> ParsedTree -> [[(Pattern, ParsedTree)]]
capture g p = everything (++) ([] `mkQ` (\ x -> case match' g p (x, []) of 
                                                Just y -> [y] 
                                                Nothing -> []))
