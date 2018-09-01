{-# LANGUAGE OverloadedStrings #-}
 module Paragraph where
import          Test.Tasty
import           Test.Tasty.HUnit
import           Data.OrgMode.Types (MarkupText (..), Paragraph (..))
import           Data.OrgMode.Parse.Attoparsec.Paragraph (parseParagraph)
import           Data.Text (pack)
import           Data.Either
import           Util

parserMarkupTests :: TestTree
parserMarkupTests = testGroup "Attoparsec orgmode Paragraph"
  [ testCase "Parses Single Markup Paragraph" $
      testDocS "* text *" $ Paragraph [Bold [(Plain . pack) " text "]],
    testCase "Parses the Plain Paragraph" $
      testDocS " text "   $ Paragraph [(Plain . pack) " text "],
    testCase "Parses the broken markup Paragraph with token at start" $
      testDocS "_ text "  $ Paragraph [(Plain . pack) "_ text "],
    testCase "Parses the broken markup Paragraph with token at end" $
      testDocS " text *"  $ Paragraph [(Plain . pack) " text *"],
    testCase "Parses the broken markup Paragraph with token in middle" $
      testDocS " te*xt "  $ Paragraph [(Plain . pack) " te*xt "],
    testCase "Parses Single Markup Paragraph" $
      testDocS "_* text *_" $ Paragraph [Italic [Bold [(Plain . pack) " text "]]],
    testCase "Parses Multi-lines Markup Paragraph" $
      testDocS "_* l1p1\nl2p2 *_" $ Paragraph [Italic [Bold [(Plain . pack) " l1p1l2p2 "]]],
    testCase "Parses Multi-lines Markup Paragraph followed a Headline" $
      testDocS "_* l1p1\nl2p2 *_\n" $ Paragraph [Italic [Bold [(Plain . pack) " l1p1l2p2 "]]],
    testCase "Parses the Headline in Paragraph" $
      testException "* text " "Not a Paragraph"
  ]
  where
    testDocS s expected = expectParse parseParagraph (pack s) (Right expected)
    testException s expected = expectParse parseParagraph (pack s) (Left expected)
