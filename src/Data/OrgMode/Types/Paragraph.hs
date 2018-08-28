-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Types.Paragraph
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.OrgMode.Types.Paragraph
( 
  MarkupText (..),
  Paragraph  (..),
)
where

import           GHC.Generics
import           Data.Semigroup       (Semigroup)
import           Data.Text             (Text)

data Markup i = Plain i | Bold [Markup i] | Italic [Markup i] deriving (Show, Eq, Generic)
newtype MarkupText = Markup Text deriving (Show, Eq, Generic)
newtype Paragraph = Paragraph [MarkupText] deriving (Show, Eq, Generic, Semigroup, Monoid)
