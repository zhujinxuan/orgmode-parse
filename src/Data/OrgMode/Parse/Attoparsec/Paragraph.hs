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
  parsePlainText
)
where

import           Control.Applicative
import           Data.Semigroup
import           Data.Functor                          (($>))
import           Data.Text                             (Text, cons, pack, append)
import           Data.Attoparsec.Text                  (Parser, satisfy, takeWhile, choice, char, anyChar, parseOnly, endOfInput)
import           Data.Maybe                            (isNothing)
import           Data.OrgMode.Types.Paragraph          (MarkupText (..), Paragraph (..))
import           Prelude                        hiding (takeWhile)

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
  return $ Plain (cons c content)

parseMarkupContent :: Parser [MarkupText]
parseMarkup :: Parser MarkupText
parseMarkupContent = (endOfInput $> []) <> (appendElement <$> parseMarkup <*> parseMarkupContent)
parseMarkup = choice (map (createTokenParser parseMarkupContent) tokens) <> parsePlainText

appendElement :: MarkupText -> [MarkupText] -> [MarkupText]
appendElement a [] = [a]
appendElement (Plain nonEmptyText) (Plain parserFailedText: xs) = Plain (append nonEmptyText parserFailedText) : xs
appendElement h t = if h == Plain (pack "") 
                       then t
                       else h:t
            

parseParagraph :: Parser Paragraph
parseParagraph = Paragraph <$> parseMarkupContent
