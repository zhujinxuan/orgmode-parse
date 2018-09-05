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

module Data.OrgMode.Parse.Attoparsec.Paragraph.Paragraph
( 
  parseParagraph,
  parsePlainText,
)
where

import           Control.Applicative            
import           Control.Monad                         (when)
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text, cons, append, snoc, intercalate)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, (<?>), many', eitherP)
import           Data.OrgMode.Types          (MarkupText (..), Paragraph (..))
import           Data.OrgMode.Parse.Attoparsec.Util    (takeLinesTill, isHeadLine, takeContentBeforeBlockTill, takeEmptyLine, feedParserText)
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Markup    (parseMarkupContent)
import           Data.OrgMode.Parse.Attoparsec.Paragraph.Item      (parseItems)

parseBlockTuple :: Parser s -> Parser ([Paragraph], Maybe s)
parseBlockTuple parseBlock = do
  (content, block) <- takeContentBeforeBlockTill isHeadLine parseBlock
  if Text.find isSpace content == null
     then return ([], block)
     else (, block) . Just <$> takeInnerTextAsParagraphs content 

takeInnerTextAsParagraphs :: Text -> Parser [Paragraph]
takeInnerTextAsParagraphs = feedParserText pa where
  pa = many' (Paragraph <$> psingle)
  psingle = eitherP parseItems pmarkup
  pmarkup = takeLinesTill isItemLine >>= feedParserText  parseMarkupContent

