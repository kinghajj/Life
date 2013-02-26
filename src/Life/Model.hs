{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Life.Model
-- Copyright   : (C) 2013 Sam Fredrickson
-- License     : BSD-style (see "LICENSE" file)
-- Maintainer  : Sam Fredrickson <kinghajj@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- The model to be simulated by the program.
--------------------------------------------------------------------------------
module Life.Model where

import Control.Lens
import Data.Monoid
import Prelude hiding (id)
import qualified Data.Set as Set

-- There are only two sexes.

data Sex = Male | Female | None deriving (Show)

-- For some reason, (^.) wants Sex to be an instance of Monoid... OK, why not?

instance Monoid Sex where
  mempty = None
  Male   `mappend` Male   = Male
  Male   `mappend` Female = None
  Female `mappend` Male   = None
  Female `mappend` Female = Female
  a      `mappend` None   = a
  None   `mappend` a      = a

instance Monoid Float where
  mempty  = 0.0
  mappend = (+)

-- There are two kinds of objects, food and organisms. Both have ids, positions,
-- sizes, and energies. Organisms also have velocities and genders.

data Object
  = Food     { _id       :: Int
             , _position :: (Float, Float)
             , _size     :: Float
             , _energy   :: Float
             }
  | Organism { _id          :: Int
             , _position    :: (Float, Float)
             , _velocity    :: (Float, Float)
             , _size        :: Float
             , _energy      :: Float
             , _metabolism  :: Float
             , _gender      :: Sex
             }
  deriving (Show)

makeLenses ''Object

isFood :: Object -> Bool
isFood (Food _ _ _ _) = True
isFood _              = False

isOrganism :: Object -> Bool
isOrganism (Organism _ _ _ _ _ _ _) = True
isOrganism _                        = False

isMale :: Object -> Bool
isMale (Organism _ _ _ _ _ _ Male) = True
isMale _                           = False

isFemale :: Object -> Bool
isFemale (Organism _ _ _ _ _ _ Female) = True
isFemale _                             = False

collide :: Object -> Object -> Bool
collide o1 o2 =
  let (x1, y1) = o1 ^. position
      (x2, y2) = o2 ^. position
      d        = sqrt ((x2-x1)**2 + (y2-y1)**2)
      rs       = o1 ^. size + o2 ^. size
  in  d < rs

-- Use objects' ids for determining equality and ordering.

instance Eq Object where
  o1 == o2 = (o1 ^. id) == (o2 ^. id)

instance Ord Object where
  compare o1 o2 = compare (o1 ^. id) (o2 ^. id)

-- The model is simply a set of objects.

type Model = Set.Set Object
