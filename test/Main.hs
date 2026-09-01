{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Syntax.Base
import Syntax.Peg
import Syntax.Pattern
import Syntax.ParsedTree
import Parser.Peg
import Match.Capture
import Parser.Base (Parser)
import Parser.Pattern (patterns)
import qualified Quote.Peg as QPeg
import qualified Quote.Pattern as QPattern

import Text.Megaparsec (parse, errorBundlePretty)

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
        testGroup "Main tests" [testsParserPeg, testsMatch, testsQuote]

-- | Run a parser and return its result, aborting the test with the actual
-- parse error if it fails.
parseT :: Parser a -> String -> a
parseT p f = case parse p "" f of
                    Right a -> a
                    Left e  -> error (errorBundlePretty e)

testsParserPeg :: TestTree
testsParserPeg = testGroup "Tests Parser Peg"
    [
        testCase "simple peg, a single rule" $
            parseT grammar "A <- \"a\"+"
            @?=
            ([(NT "A",Sequence (ExprT (T "a")) (Star (ExprT (T "a"))))],NT "A")

    ,   testCase "expression peg, with a non-terminal for numbers" $
            parseT grammar "E <- T (\"\\\"\" T)*\nT <- F (\"*\" F)*\nF <- \"num\" / \"(\" E \")\""
            @?=
            ([(NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "\"")) (ExprNT (NT "T"))))),(NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),(NT "F",Choice (ExprT (T "num")) (Sequence (ExprT (T "(")) (Sequence (ExprNT (NT "E")) (ExprT (T ")")))))],NT "E")

    ,   testCase "expression peg, with a character range for numbers" $
            parseT grammar "E <- T (\"+\" T)*\nT <- F (\"*\" F)*\nF <- [0-9]+ / \"(\" E \")\""
            @?=
            ([
                (NT "E",Sequence (ExprNT (NT "T")) (Star (Sequence (ExprT (T "+")) (ExprNT (NT "T"))))),
                (NT "T",Sequence (ExprNT (NT "F")) (Star (Sequence (ExprT (T "*")) (ExprNT (NT "F"))))),
                (NT "F",
                    Choice
                        (Sequence
                            (Choice (ExprT (T "0")) (Choice (ExprT (T "1")) (Choice (ExprT (T "2")) (Choice (ExprT (T "3")) (Choice (ExprT (T "4")) (Choice (ExprT (T "5")) (Choice (ExprT (T "6")) (Choice (ExprT (T "7")) (Choice (ExprT (T "8")) (ExprT (T "9")))))))))))
                            (Star (Choice (ExprT (T "0")) (Choice (ExprT (T "1")) (Choice (ExprT (T "2")) (Choice (ExprT (T "3")) (Choice (ExprT (T "4")) (Choice (ExprT (T "5")) (Choice (ExprT (T "6")) (Choice (ExprT (T "7")) (Choice (ExprT (T "8")) (ExprT (T "9")))))))))))))
                        (Sequence
                            (ExprT (T "("))
                            (Sequence (ExprNT (NT "E")) (ExprT (T ")")))))],NT "E")
    ]

-- | Grammar used by the matching tests.
--
-- The trees in 'testsMatch' are built by hand rather than produced by the
-- parser, so this grammar only needs to justify the subtree bound to a
-- variable: @F@ derives the factor @2 * 3@. 'match' consults the grammar
-- only in the 'PatVar' case, via 'ofExpression'.
matchGrammar :: Grammar
matchGrammar =
    ( [ (NT "F", Sequence (ExprT (T "2")) (Sequence (ExprT (T "*")) (ExprT (T "3")))) ]
    , NT "F"
    )

testsMatch :: TestTree
testsMatch = testGroup "Tests Match"
    [
        testCase "Match Epsilon" $
            match matchGrammar PatEpsilon ParsedEpsilon
            @? "PatEpsilon should match ParsedEpsilon"

        -- 'match' searches every subtree, so a nested epsilon is a match.
        -- This test used to assert the opposite, back when 'match' was
        -- anchored at the root; see the examples on 'match'.
    ,   testCase "Match Nested Epsilon" $
            match matchGrammar
                PatEpsilon
                (ParsedSeq ParsedEpsilon (ParsedT (T "test")))
            @? "PatEpsilon should be found in the nested epsilon of the sequence"
    ,   testCase "Match expression tree" $
            match matchGrammar
                (PatSeq    (PatT    (T "1")) (PatSeq    (PatSeq    (PatT    (T "+")) (PatVar (ExprNT (NT "F")) "Test"))                                                                  (PatSeq    (PatT    (T "+")) (PatT    (T "4")))))
                (ParsedSeq (ParsedT (T "1")) (ParsedSeq (ParsedSeq (ParsedT (T "+")) (ParsedNT (NT "F") (ParsedSeq (ParsedT (T "2")) (ParsedSeq (ParsedT (T "*")) (ParsedT (T "3")))))) (ParsedSeq (ParsedT (T "+")) (ParsedT (T "4")))))
            @? "the variable bound to F should match the 2 * 3 subtree"
    ,   testCase "Match with itself" $
            match matchGrammar
                (PatSeq    (PatT    (T "1")) (PatSeq    (PatSeq    (PatT    (T "+")) (PatNT    (NT "F") (PatSeq    (PatT    (T "2")) (PatSeq    (PatT    (T "*")) (PatT    (T "3")))))) (PatSeq    (PatT    (T "+")) (PatT    (T "4")))))
                (ParsedSeq (ParsedT (T "1")) (ParsedSeq (ParsedSeq (ParsedT (T "+")) (ParsedNT (NT "F") (ParsedSeq (ParsedT (T "2")) (ParsedSeq (ParsedT (T "*")) (ParsedT (T "3")))))) (ParsedSeq (ParsedT (T "+")) (ParsedT (T "4")))))
            @? "a pattern should match the tree with the same shape"
    ]

-- The values below used to be exported by Pipeline.MatchPipeline as sample
-- data. They are the only place where the quasi-quoters are exercised, so they
-- live here instead, paired with the equivalent source text: each test asserts
-- that the quasi-quoter and the runtime parser agree on the same input.

expressionSource :: String
expressionSource = unlines
    [ "E <- T (\"+\" T)*"
    , "T <- F (\"*\" F)*"
    , "F <- n / \"(\" E \")\""
    , "^n <- [0-9]+"
    ]

quotedExpressionGrammar :: Grammar
quotedExpressionGrammar = [QPeg.grammar|
E <- T ("+" T)*
T <- F ("*" F)*
F <- n / "(" E ")"
^n <- [0-9]+
|]

wikiSource :: String
wikiSource = unlines
    [ "S <- \"x\" S \"x\" / \"x\"" ]

quotedWikiGrammar :: Grammar
quotedWikiGrammar = [QPeg.grammar|
S <- "x" S "x" / "x"
|]

callGraphSource :: String
callGraphSource = unlines
    [ "pattern call : function_call := #name:identifier @space \"(\" @space #v:(expr_list?) \")\" ε"
    , ""
    , "pattern definition : function_def := (\"def\" @space #name:identifier \"(\" @space #p:(id_list?) \")\" @space \":\") #block:(statement*)"
    , ""
    , "pattern space : space := \" \"*"
    ]

quotedCallGraphPatterns :: [NamedSynPat]
quotedCallGraphPatterns = [QPattern.patterns|
pattern call : function_call := #name:identifier @space "(" @space #v:(expr_list?) ")" ε

pattern definition : function_def := ("def" @space #name:identifier "(" @space #p:(id_list?) ")" @space ":") #block:(statement*)

pattern space : space := " "*
|]

testsQuote :: TestTree
testsQuote = testGroup "Tests QuasiQuoters"
    [ testCase "peg quoter agrees with the parser (expressions)" $
        quotedExpressionGrammar @?= parseT grammar expressionSource

    , testCase "peg quoter agrees with the parser (wiki)" $
        quotedWikiGrammar @?= parseT grammar wikiSource

    , testCase "pattern quoter agrees with the parser (call graph)" $
        quotedCallGraphPatterns @?= parseT patterns callGraphSource
    ]
