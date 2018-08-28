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

module Data.OrgMode.Types.Paragraph
( 
  MarkupText (..),
  Paragraph  (..),
)
where

import           Control.Applicative
import           Data.Semigroup

data Markup i = Plain i | Bold [Markup i] | Italic [Markup i] deriving (Show, Eq, Generic)
newtype MarkupText = Markup Text
newtype Paragraph = Paragraph [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)
