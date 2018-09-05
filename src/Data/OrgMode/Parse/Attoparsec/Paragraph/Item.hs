-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Item
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}


module Data.OrgMode.Parse.Attoparsec.Paragraph
( 
  parseItems
)
where

import           Control.Applicative            
import           Control.Monad                         (when)
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, takeWhile, choice, char, anyChar, parseOnly, isEndOfLine, endOfInput, manyTill, (<?>), many1', atEnd)
import           Data.OrgMode.Types                    (Item)
import           Data.OrgMode.Parse.Attoparsec.Util    (takeALine, takeLinesTill, takeEmptyLine)
import           Data.Maybe                            (isNothing)
import           GHC.Generics
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Markup   (parseMarkupContent)

data ItemStart = ItemStart { prefixLength :: Int, firstLine :: Text} deriving (Show, Eq, Generic, Semigroup, Monoid)

parseItemStart :: Parser Maybe ItemP
parseItemStart = do
  content <- takeALine
  result where 
      (prefix, content) = span isSpace content
      prefixLength = Text.length prefix
      result = if prefixLength > 2 
                  then (ItemP prefixLength <$> hasFirstLine ) <?> "not an item start"
                  else fail "not an item start"
      hasFirstLine content = if Text.null content || head content /= '*'
                                then fail ""
                                else return $ dropWhile isSpace . tail content

hasMorePrefixSpaceThan :: Int -> Text -> Bool
hasMorePrefixSpaceThan i str = Text.compareLength str i == GT && isNothing (find (not . isSpace) (take i+1 str))

parseItem :: Parser Item
parseItem = do 
  itemStart <- parseItemStart
  lines <- takeLineTill hasMorePrefixSpaceThan (prefixLength itemStart) <> return ""
  Item <$> feedTextToParser (Text.append (firstLine itemStart) lines) parseMarkupContent

parseItems = Parser [Item]
parseItems = many1' parseItem
