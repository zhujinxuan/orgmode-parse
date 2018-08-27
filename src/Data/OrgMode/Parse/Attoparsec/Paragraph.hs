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
import           Data.Text                             (Text)
import           Data.Attoparsec.Text                  (Parser, satisfy, takeTill, choice)
import           Data.List                             (find)
import           Data.Maybe                            (isNothing)

data Markup i = Plain i | Bold [Markup i] | Italic [Markup i] deriving (Show, Eq, Generic)
newtype MarkupText = Markup Text
newtype Paragraph = Paragraph [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)
data Token = Token { keyChar :: Char, markup :: [MarkupText] -> MarkupText} deriving (Show, Eq, Generic)

tokens :: [Token]
tokens = [ Token '*' Bold, Token '_' Italic ]

isNotToken :: Char -> Bool
isNotToken c = c !== '*' (&&) c !== '_'


combine :: [MarkupText] -> Paragraph
combine :: xs = Paragraph $ foldr appendElement [] xs

appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement (Plain "") xs = xs
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (nonEmptyText ++ parserFailedText) : xs
appendElement h t = h:t

createTokenParser :: (Parser Text -> Parser MarkupText) -> Token -> Parser MarkupText
createTokenParser innerParser token = do
  c <- char (keyChar token)
  content <- innerParser (takeWhile1 (!== keyChar token))
  c <- char (keyChar token)
  return (markup token) content

-- Parse the Plain 
parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  content <- takeWhile isNotToken
  return $Plain (c ++ content)

parseMarkup :: Parser MarkupText
parseMarkup = choice (map tokens (createTokenParser parseMarkup) ++ [parsePlainText])

