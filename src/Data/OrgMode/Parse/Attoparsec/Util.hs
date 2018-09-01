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

import           Data.Semigroup hiding (option)
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text  (Parser, takeTill, isEndOfLine, anyChar, endOfLine, notChar, manyTill, skipSpace, option)
import           Data.Text             (Text, cons, empty, snoc, find)
import qualified Data.Text             as Text
import           Data.Char             (isSpace)
import           Data.Functor          (($>))
import           Data.Maybe            (isNothing, isJust)
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
  option content (snoc content <$> anyChar)

takeParagraphLinesTill :: (Text -> Bool) -> Parser [Text]
takeParagraphLinesTill p = do 
  content <- takeALine
  case find (not . isSpace) content of
    Nothing -> return [content]
    Just _ -> 
      if p content 
         then fail "Not a Paragraph Line"
         else (content : ) <$> (takeParagraphLinesTill p <> return []) 

-- Whether the content is ended by *text* or :text:, is used to handle isDrawer and isHeadLine
isLastSurroundBy :: Char -> Text -> Bool
isLastSurroundBy c content = case Text.split ( == c) content of
      [_, x, y] -> (not . Text.null) x && (Text.null y || Text.all isSpace y)
      _ -> False

isHeadLine :: Text -> Bool
isHeadLine content = (not . Text.null) content && Text.head content == '*' && not (isLastSurroundBy '*' content)

isDrawer :: Text -> Bool
isDrawer content = 
  case find (not .isSpace) content of
    Nothing -> False
    Just c -> c == ':' && isLastSurroundBy ':' content

takeNonEmptyLines :: Parser [Text]
takeNonEmptyLines = takeParagraphLinesTill (\c -> isDrawer c || isHeadLine c)
