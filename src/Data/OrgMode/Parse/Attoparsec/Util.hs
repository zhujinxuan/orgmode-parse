{-|
Module      :  Data.OrgMode.Parse.Attoparsec.Util
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities.
-}

module Data.OrgMode.Parse.Attoparsec.Util
( skipOnlySpace,
  nonHeadline,
  takeALine,
  takeNonEmptyLines,
)
where

import           Data.Semigroup
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text (Parser, takeTill, isEndOfLine, anyChar, endOfLine, notChar, manyTill, skipSpace, endOfInput, (<?>))
import           Data.Text             (Text, cons, empty, snoc)
import           Data.Functor          (($>))
-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Parser ()
skipOnlySpace = Attoparsec.Text.skipWhile spacePred
  where
    spacePred s = s == ' ' || s == '\t'

-- | Parse a non-heading line of a section.
nonHeadline :: Parser Text
nonHeadline = nonHeadline0 <> nonHeadline1
  where
    nonHeadline0 = endOfLine $> empty
    nonHeadline1 = cons <$> notChar '*' <*> (takeTill isEndOfLine <* endOfLine)

takeALine :: Parser Text
takeALine = do
  content <- takeTill isEndOfLine 
  c <- (endOfInput $> Nothing) <> (Just <$> anyChar)
  case c of 
    Nothing -> return content
    Just cc -> return $snoc content cc


takeNonEmptyLines :: Parser [Text]
takeNonEmptyLines = manyTill takeALine (endOfInput <> (skipSpace *> endOfLine)) <?> "Take Many Lines"
