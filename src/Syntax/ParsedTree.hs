{-|
Module      : Syntax.ParsedTree
Description : Representation of parsed trees.
Copyright   : (c) Guilherme Drummond, Rodrigo Ribeiro, 2025
License     : BSD-3-Clause
Maintainer  : rodrigo.ribeiro@ufop.edu.br
Stability   : experimental
Portability : POSIX

This module defines the structure of a parsed tree ('ParsedTree') and
associated functions, such as the 'flatten' function to extract the terminals from a tree.
It also provides an instance of the 'Pretty' class for formatted printing.
-}
module Syntax.ParsedTree
    ( ParsedTree(..)
    , ParsedTreeZipper
    , ParsedTreePath
    , ParsedTreeCrumbs
    , flatten
    , goUp
    , goDown
    , goLeft
    , goRight
    , pullFromRight
    , ofExpression
    ) where

import Syntax.Base (Terminal(..), NonTerminal, Pretty(..))
import Text.PrettyPrint.HughesPJ (text, Doc, (<+>), (<>), empty, lbrack, rbrack, hcat)
import Prelude hiding ((<>))
import Data.Generics (Data, Typeable, mkQ, everything)
import Syntax.Peg (Expression(..), Grammar, expression)

{-|
Represents an abstract syntax tree (AST).

A 'ParsedTree' can be:
- 'ParsedEpsilon': Represents the empty tree (ε).
- 'ParsedT': A terminal symbol.
- 'ParsedNT': A non-terminal symbol associated with a subtree.
- 'ParsedSeq': A sequence of two trees.
- 'ParsedChoiceLeft': Represents the left choice in a choice operation.
- 'ParsedChoiceRight': Represents the right choice in a choice operation.
- 'ParsedStar': Represents a repetition of zero or more times of a tree.
- 'ParsedNot': Represents the negation of a tree.
- 'ParsedIndent': Represents that a list of trees must be indented with respect to another tree.

@since 1.0.0
-}
data ParsedTree
    = ParsedEpsilon
    | ParsedT Terminal
    | ParsedNT NonTerminal ParsedTree
    | ParsedSeq ParsedTree ParsedTree
    | ParsedChoiceLeft ParsedTree
    | ParsedChoiceRight ParsedTree
    | ParsedStar [ParsedTree]
    | ParsedNot
    | ParsedIndent ParsedTree [ParsedTree]
    deriving (Show, Typeable, Data)

{-|
Breadcrumbs used to reconstruct the parent context while navigating a
'ParsedTree' with a zipper.

Each constructor records the information required to rebuild the tree when
moving back up from the current focus.
-}
data ParsedTreeCrumbs
    = ParsedNTCrumb NonTerminal
    | ParsedSeqFirst ParsedTree
    | ParsedSeqSecond ParsedTree
    | ParsedChoiceLeftCrumb
    | ParsedChoiceRightCrumb
    | ParsedStarCrumb [ParsedTree] [ParsedTree] -- first list is what remains, second is what has already been visited
    | ParsedIndentFirst [ParsedTree]
    | ParsedIndentSecond ParsedTree

{-|
A zipper path is the list of breadcrumbs representing the current position
inside a 'ParsedTree'. The most recent breadcrumb is at the head of the list.
-}
type ParsedTreePath = [ParsedTreeCrumbs]

{-|
A zipper for a parsed tree. The first component is the current focus, and the
second component is the path back to the root.
-}
type ParsedTreeZipper = (ParsedTree, ParsedTreePath)

{-|
Move the focus of a 'ParsedTreeZipper' up to its parent node, if possible.

This reconstructs the parent node from the current focus and the breadcrumb
stored in the zipper path.
-}
goUp :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goUp (t, ParsedNTCrumb nt:z)                 = Just (ParsedNT nt t, z)
goUp (t, ParsedSeqFirst t':z)                = Just (ParsedSeq t t', z)
goUp (t, ParsedSeqSecond t':z)               = Just (ParsedSeq t' t, z)
goUp (t, ParsedChoiceLeftCrumb:z)            = Just (ParsedChoiceLeft t, z)
goUp (t, ParsedChoiceRightCrumb:z)           = Just (ParsedChoiceRight t, z)
-- goUp (t, (ParsedStarCrumb ts []):z)          = Just (ParsedStar (t:ts), z)
-- goUp z@(_, (ParsedStarCrumb _ _):_)          = goUp =<< goLeft z
goUp (t, ParsedStarCrumb ts1 ts2:z)          = Just (ParsedStar (t : reverse ts2 ++ ts1), z)
goUp (t, ParsedIndentFirst ts:z)             = Just (ParsedIndent t ts, z)
goUp (ParsedStar ts, ParsedIndentSecond t:z) = Just (ParsedIndent t ts, z)
goUp (_, ParsedIndentSecond _:_)             = Nothing
goUp (_, [])                                 = Nothing

-- TODO:
-- goLeft, goRight and goDown do not behave well when reaching for the left and
-- right of a tree that sits inside a list, because moving along the list takes
-- precedence.

{-|
Move the focus down into a child subtree, when the current focus is a node
that contains a single child or a non-empty star list.
-}
goDown :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goDown (ParsedNT nt t, z)       = Just (t, ParsedNTCrumb nt:z)
goDown (ParsedChoiceLeft t, z)  = Just (t, ParsedChoiceLeftCrumb:z)
goDown (ParsedChoiceRight t, z) = Just (t, ParsedChoiceRightCrumb:z)
goDown (ParsedStar [], _)       = Nothing
goDown (ParsedStar (t:ts), z)   = Just (t, ParsedStarCrumb ts []:z)
goDown _                        = Nothing

{-|
Move the focus left within the current zipper context.

This is valid for star lists, sequence nodes, and indent nodes where a
left sibling exists.
-}
goLeft :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goLeft (_, (ParsedStarCrumb _ []):_)         = Nothing
goLeft (t, (ParsedStarCrumb ts1 (t':ts2)):z) = Just (t', ParsedStarCrumb (t:ts1) ts2:z)
goLeft (ParsedSeq t1 t2, z)                  = Just (t1, ParsedSeqFirst t2:z)
goLeft (ParsedIndent t ts, z)                = Just (t, ParsedIndentFirst ts:z)
goLeft _                                     = Nothing

{-|
Move the focus right within the current zipper context.

This is valid for star lists, sequence nodes, and indent nodes where a
right sibling exists.
-}
goRight :: ParsedTreeZipper -> Maybe ParsedTreeZipper
goRight (_, (ParsedStarCrumb [] _):_)         = Nothing
goRight (t, (ParsedStarCrumb (t':ts1) ts2):z) = Just (t', ParsedStarCrumb ts1 (t:ts2):z)
goRight (ParsedSeq t1 t2, z)                  = Just (t2, ParsedSeqSecond t1:z)
goRight (ParsedIndent t ts, z)                = Just (ParsedStar ts, ParsedIndentSecond t:z)
goRight _                                     = Nothing

{-|
Pull the first element from the right side of a sequence and append it to
its left side.

If the provided tree is not a sequence, this returns 'Nothing'.
-}
pullFromRight :: ParsedTree -> Maybe ParsedTree
pullFromRight (ParsedSeq t1 t2) = maybe (Just t1') (Just . ParsedSeq t1') tT
    where
        (tH, tT) = getHead t2
        t1' = addAtEnd t1 tH
pullFromRight _ = Nothing

addAtEnd :: ParsedTree -> ParsedTree -> ParsedTree
addAtEnd (ParsedSeq t1 t2) e3 = ParsedSeq t1 $ addAtEnd t2 e3
addAtEnd e e3 = ParsedSeq e e3

getHead :: ParsedTree -> (ParsedTree, Maybe ParsedTree)
getHead (ParsedSeq t1 t2) = (t1, Just t2)
getHead e                = (e, Nothing)

{-|
Instance of the 'Pretty' class for 'ParsedTree'.

Prints the syntax tree in a readable format, with indentation
and visual symbols to represent the tree hierarchy.

@since 1.0.0
-}
instance Pretty ParsedTree where
    pPrint :: ParsedTree -> Doc
    pPrint pt = pPrint' Text.PrettyPrint.HughesPJ.empty pt <> text "\n"

-- Auxiliary functions for tree formatting
nest :: Doc -> Doc
nest i = i <> text "├╴"

nest1 :: Doc -> Doc
nest1 i = i <> text "╰╴"

continue :: Doc -> Doc
continue i = i <> text "| "

continue1 :: Doc -> Doc
continue1 i = i <> text "  "

{-|
Auxiliary function for formatted printing of a 'ParsedTree'.

@since 1.0.0
-}
pPrint' :: Doc -> ParsedTree -> Doc
pPrint' _ ParsedEpsilon = text "ε"
pPrint' _ (ParsedT t) = pPrint t
pPrint' indent (ParsedNT nt tree) =
    text "NT" <+> pPrint nt <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedSeq t1 t2) =
    text "Seq" <> text "\n"
    <> nest indent <> pPrint' (continue indent) t1 <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) t2
pPrint' indent (ParsedChoiceLeft tree) =
    text "Left" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedChoiceRight tree) =
    text "Right" <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) tree
pPrint' indent (ParsedStar ts) =
    text "Star" <+> lbrack <> list' <> rbrack
    where
        listnest = if null ts then Text.PrettyPrint.HughesPJ.empty else text "\n"
        listEnd   = if null ts then Text.PrettyPrint.HughesPJ.empty else nest1 indent
        list      = hcat (map (\ x -> nest indent <> pPrint' (continue indent) x <> text "\n") ts)
        list'     = listnest <> list <> listEnd
pPrint' _ ParsedNot = Text.PrettyPrint.HughesPJ.empty
pPrint' indent (ParsedIndent e b) =
    text "Indent" <> text "\n"
    <> nest indent <> pPrint' (continue indent) e <> text "\n"
    <> nest1 indent <> pPrint' (continue1 indent) (ParsedStar b)

{-|
Extracts all terminal symbols from a 'ParsedTree' as a single string.

=== Usage examples:

>>> flatten (ParsedSeq (ParsedT (T "a")) (ParsedT (T "b")))
"ab"

>>> flatten ParsedEpsilon
""

@since 1.0.0
-}
flatten :: ParsedTree -> String
flatten = everything (++) ("" `mkQ` term)
    where
        term (ParsedT (T t)) = t
        term _ = ""

{-|
Check whether a 'ParsedTree' corresponds to a given grammar expression.

The function follows the structure of the expression and compares it with the
parsed tree, resolving non-terminals using the provided grammar.
-}
ofExpression :: Grammar -> Expression -> ParsedTree -> Bool
ofExpression _ Empty ParsedEpsilon = True
ofExpression _ (ExprT t) (ParsedT t') = t == t'
ofExpression g (ExprNT nt) (ParsedNT nt' t) = nt == nt' && case expression g nt of
                                                Just x -> ofExpression g x t
                                                Nothing -> False
ofExpression g (Sequence e1 e2) (ParsedSeq t1 t2) = ofExpression g e1 t1 && ofExpression g e2 t2
ofExpression g (Choice e1 _) (ParsedChoiceLeft t) = ofExpression g e1 t
ofExpression g (Choice _ e2) (ParsedChoiceRight t)  = ofExpression g e2 t
ofExpression g (Star e) (ParsedStar ts) = all (ofExpression g e) ts
ofExpression _ (Not _) ParsedNot = True
ofExpression _ (Flatten _) (ParsedT _) = True 
ofExpression g (Indent e1 e2) (ParsedIndent t ts) = ofExpression g e1 t && all (ofExpression g e2) ts
ofExpression _ _ _ = False