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

{-# LANGUAGE RecordWildCards   #-}

module Data.OrgMode.Parse.Attoparsec.Paragraph
( 
)
where

import           Control.Applicative
import           Data.Semigroup
import           Data.Text                             (Text, cons)
import           Data.Attoparsec.Text                  (Parser, satisfy, takeTill, choice, anyChar, parseOnly, atEnd)
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

createTokenParser :: Parser [MarkupText] -> Token -> Parser MarkupText
createTokenParser innerParser Token{..}= do 
  _ <- char keyChar
  content <- takeWhile (!== keyChar) 
  _ <- char keyChar
  case parseOnly innerParser content of
     Left s -> fail s
     Right a -> return a

parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  content <- takeWhile isNotToken
  return $ Plain (cons c content)

parseMarkupContent :: Parser [MarkupText]
parseMarkup :: Parser MarkupText
parseMarkupContent = atEnd <> (((:) <$> parseMarkup <*> parseMarkupContent) >>= combine)
parseMarkup = choice (map tokens (createTokenParser parseMarkupContent) ++ [parsePlainText])

combine :: [MarkupText] -> Paragraph
combine :: xs = Paragraph $ foldr appendElement [] xs

appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement (Plain "") xs = xs
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (nonEmptyText ++ parserFailedText) : xs
appendElement h t = h:t

parseParagraph :: Parser Paragraph
parseParagraph = do
  content <- parseMarkupContent
  return Paragraph content
