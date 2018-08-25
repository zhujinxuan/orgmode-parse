-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Paragraph
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------


module Data.OrgMode.Parse.Attoparsec.Paragraph
( 
)
where

import           Control.Applicative
import           Data.Semigroup
import  Data.Attoparsec.Text (char, takeUntil)

-- Markup Structure
data Markup i = Plain i | Bold [Markup i] | Italic [Markup i] deriving (Show, Eq, Generic)
newtype MarkupText = Markup Text
newtype Paragraph = Paragraph [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)

-- Parse a markup text
createMarkupParser :: (Text -> MarkupText ) -> Char -> Parser MarkupText
createMarkupParser t c = do
  _ <- char c
  content <- takeUntil (== c)
  _ <- char c
  return $ t content

combine :: [MarkupText] -> Paragraph
combine :: xs = Paragraph $ foldr appendElement [] xs

appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement (Plain "") xs = xs
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (nonEmptyText ++ parserFailedText) : xs
appendElement h t = h:t

parserPlainTextFallback tokens = do
  a <- anyChar
  b <- takeUtil (`elem` tokens)
  return Plain (a:b)
