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
  takeLinesTill,
  isHeadLine
)
where

import           Data.Semigroup hiding (option)
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text  (Parser, takeTill, isEndOfLine, anyChar, endOfLine, notChar, manyTill, skipSpace, option, isHorizontalSpace)
import           Data.Text             (Text, cons, empty, snoc, find)
import qualified Data.Text             as Text
import           Data.Char             (isSpace)
import           Data.Functor          (($>))
-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Parser ()
skipOnlySpace = Attoparsec.Text.skipWhile isHorizontalSpace

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

takeLinesTill :: (Text -> Bool) -> Parser Text
takeLinesTill p = takePLines where
  takePLines = do 
    content <- takeALine
    case find (not . isSpace) content of
      Nothing -> return empty
      Just _ -> 
        if p content 
           then fail "Not a Paragraph Line"
           else Text.append content <$> (takeLinesTill p <> return empty) 

-- Whether the content is ended by *text* or :text:, is used to handle isDrawer and isHeadLine
isLastSurroundBy :: Char -> Text -> Bool
isLastSurroundBy c content = case Text.split ( == c) content of
      [_, x, y] -> (not . Text.null) x && (Text.null y || Text.all isSpace y)
      _ -> False

isHeadLine :: Text -> Bool
isHeadLine content = (not . Text.null) content && Text.head content == '*' && not (isLastSurroundBy '*' content)
