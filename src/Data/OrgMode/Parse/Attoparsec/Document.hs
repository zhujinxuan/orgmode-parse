module Data.OrgMode.Parse.Attoparsec.Document (
 parseDocument
) where

import           Control.Applicative                     ((<$>), (<*>))
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                   as TP
import           Prelude                                 hiding (unlines)
import           Data.Text                               (Text, pack, unlines)
import           Data.OrgMode.Parse.Types
import           Data.OrgMode.Parse.Attoparsec.Headings
import           Data.OrgMode.Parse.Attoparsec.Section (nonHeaderLine)

------------------------------------------------------------------------------
parseDocument :: [Text] -> TP.Parser Text Document
parseDocument otherKeywords =
  Document
    <$> (unlines <$> many' nonHeaderLine)
    <*> many' (headingBelowLevel otherKeywords 0)
