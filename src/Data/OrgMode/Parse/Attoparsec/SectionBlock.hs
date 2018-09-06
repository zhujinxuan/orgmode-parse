-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.SectionBlock
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE TupleSelections        #-}

module Data.OrgMode.Parse.Attoparsec.SectionBlock
( 
  parseBlockTuple
)
where

import           Control.Applicative            
import           Control.Monad                         (when)
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text, cons, append, snoc, intercalate)
import qualified Data.Text                      as     Text
import           Data.Maybe                            (isJust, isNothing)
import           Data.Attoparsec.Text                  (Parser, (<?>), many', eitherP)
import           Data.OrgMode.Types                    (SectionBlock (..))

import           Data.OrgMode.Parse.Attoparsec.Util                (takeLinesTill, isHeadLine, takeContentBeforeBlockTill, takeEmptyLine, feedParserText, isEmptyLine)
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Markup    (parseMarkupContent)
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Item      (parseList)

parseBlockAndDrawer :: Parser s -> Parser ([SectionBlock], Maybe s)
parseBlockAndDrawer parseDrawer = do
  (content, drawer) <- takeContentBeforeBlockTill isHeadLine parseDrawer
  if isEmptyLine content
     then return ([], drawer)
     else (, drawer) . Just <$> takeInnerTextAsParagraphs content 

innerTextToBlocks :: Text -> Parser [SectionBlock]
innerTextToBlocks = feedTextToParser parseBlocks where
  parseBlocks = concat <$> many' parseBlock
  parseBlock =  appendParagraphAndList <$> takeContentBeforeBlockTill isHeadLine parseList
  appendParagraphAndList :: (Text, Maybe List) -> Parser [SectionBlock]
  appendParagraphAndList (text, list) = (++) <$> fetchParagraph text <*> fetchList list
  fetchParagraph :: Text  -> Parser [SectionBlock]
  fetchParagraph content
    | isEmptyLine content = return []
    | otherwise = [] . Left . Paragraph <$> feedParserText parseMarkupContent content
  fetchList ::  Maybe List -> Parser [SectionBlock]
  fetchList  (_, Nothing) = return []
  fetchList  (_, Just x) = return [Left x]
