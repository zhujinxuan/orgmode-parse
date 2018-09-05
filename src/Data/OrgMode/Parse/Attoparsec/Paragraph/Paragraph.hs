-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Paragraph.Paragraph
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

parseParagraph :: Parser Paragraph
parseParagraph = many' takeEmptyLine *> parseContent where
  parseContent =  takeContentTillHeadline >>= takeTextAsParagraph
  takeContentTillHeadline = takeLinesTill isHeadLine <?> "Not a paragraph line"

takeParagraphAndBlock :: Parser s -> Parser (Maybe Paragraph, Maybe s)
takeParagraphAndBlock parseBlock = do
  (content, block) <- takeContentBeforeBlockTill isHeadLine parseBlock
  let paragraphText = Text.dropWhileEnd isSpace content in 
      if Text.null paragraphText 
         then return (Nothing, block)
         else (, block) . Just <$> takeTextAsParagraph content 

takeTextAsParagraph :: Text -> Parser Paragraph
takeTextAsParagraph = feedParserText (Paragraph <$> eitherP parseItems parseMarkupContent)
