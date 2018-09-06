{-# LANGUAGE OverloadedStrings #-}

module Paragraph where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types (MarkupText (..))
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Markup (parseMarkupContent)
import           Data.Either
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses a Single Markup" $
      testDocS "* text *"  [Bold [Plain  " text "]],
    testCase "Parses a Plain Text" $
      testDocS " text "    [Plain " text"],
    testCase "Parses a broken markup with token at start" $
      testDocS "_ text "   [Plain "_ text"],
    testCase "Parses a broken markup Paragraph with token at end" $
      testDocS " text *"   [Plain " text *"],
    testCase "Parses a broken markup Paragraph with token in middle" $
      testDocS " te*xt "   [Plain   " te*xt"],
    testCase "Parses Nested Markup" $
      testDocS "_* text *_"  [Italic [Bold [Plain  " text "]]],
    testCase "Paragraph Parser shall not try to parse markup across lines" $
      testDocS " _* l1p1 \nl2p2 *_"  [Plain  "_* l1p1 l2p2 *_"],
    testCase "Paragraph Parser shall ignore the space before endOfLine (in plain)" $
      testDocS " l1p1 \nl2p2 "  [Plain  " l1p1 l2p2"],
    testCase "Paragraph Parser shall stop at the empty line" $
      testDocS "l1p1 \n\nl2p2 "  [Plain  "l1p1"]
  ]
  where
    testDocS s expected = expectParse parseMarkupContent s (Right expected)
    testException s expected = expectParse parseMarkupContent  s (Left expected)
