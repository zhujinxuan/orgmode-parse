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
  parseParagraph,
  parsePlainText,
)
where

import           Control.Applicative            
import           Data.Semigroup
import           Data.Text                             (cons, append, filter)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, takeWhile, choice, char, anyChar, parseOnly, isEndOfLine, endOfInput, manyTill, (<?>))
import           Data.OrgMode.Types          (MarkupText (..), Paragraph (..))
import           Prelude                        hiding (takeWhile, filter)
import           Data.OrgMode.Parse.Attoparsec.Util    (takeLinesTill, isHeadLine)

data Token = Token { keyChar :: Char, markup :: [MarkupText] -> MarkupText} 

tokens :: [Token]
tokens = [ Token '*' Bold, Token '_' Italic ]

isNotToken :: Char -> Bool
isNotToken c = c /= '*' && c /= '_'

createTokenParser :: Parser [MarkupText] -> Token -> Parser MarkupText
createTokenParser innerParser Token{..}= do 
  _ <- char keyChar
  content <- takeWhile (/= keyChar) 
  _ <- char keyChar
  case parseOnly innerParser content of
     Left s -> fail s
     Right a -> return $ markup a

parsePlainText :: Parser MarkupText
parsePlainText = do
  c <- anyChar
  content <- takeWhile isNotToken
  return $ Plain (filter (not . isEndOfLine) (cons c content))

parseMarkupContent :: Parser [MarkupText]
parseMarkup :: Parser MarkupText
parseMarkupContent =  foldr appendElement [] <$> manyTill parseMarkup endOfInput  
parseMarkup = choice (map (createTokenParser parseMarkupContent) tokens) <> parsePlainText

emptyMarkup :: MarkupText
emptyMarkup = Plain Text.empty
appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement a [] = [a]
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (append nonEmptyText parserFailedText) : xs
appendElement h t
  | h == emptyMarkup = t
  | head t == emptyMarkup = h: tail t
  | otherwise = h:t

parseParagraph :: Parser Paragraph
parseParagraph = do
  text <- takeLinesTill isHeadLine <?> "Not a paragraph line"
  case parseOnly parseMarkupContent text of 
    Left s -> fail s
    Right s -> return $ Paragraph s

