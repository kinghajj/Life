{-# LANGUAGE TemplateHaskell #-}
module Life.Model where

import Control.Lens
import Data.Monoid
import Graphics.Gloss.Interface.Pure.Simulate
import Prelude hiding (id)
import qualified Data.Set as Set

-- There are only two sexes.

data Sex = Male | Female | None deriving (Show)

-- For some reason, (^.) wants Sex to be an instance of Monoid... OK, why not?

instance Monoid Sex where
  mempty = None
  Male `mappend` Male     = Male
  Male `mappend` Female   = None
  Female `mappend` Male   = None
  Female `mappend` Female = Female
  a `mappend` None        = a
  None `mappend` a        = a

-- There are two kinds of objects, food and organisms. Both have ids, positions,
-- sizes, and energies. Organisms also have velocities and genders.

data Object
  = Food     { _id       :: Int
             , _position :: (Float, Float)
             , _size     :: Float
             , _energy   :: Float
             }
  | Organism { _id       :: Int
             , _position :: (Float, Float)
             , _velocity :: (Float, Float)
             , _size     :: Float
             , _energy   :: Float
             , _gender   :: Sex
             }
  deriving (Show)

makeLenses ''Object

isFood :: Object -> Bool
isFood (Food _ _ _ _) = True
isFood _              = False

isOrganism :: Object -> Bool
isOrganism (Organism _ _ _ _ _ _) = True
isOrganism _                      = False

-- Use objects' ids for determining equality and ordering.

instance Eq Object where
  o1 == o2 = (o1 ^. id) == (o2 ^. id)

instance Ord Object where
  compare o1 o2 = compare (o1 ^. id) (o2 ^. id)

-- The model is simply a set of objects.

type Model = Set.Set Object

initialModel :: Model
initialModel = Set.fromList [
  Food 0 (100, 100) 20 10
  ]

stepModel :: ViewPort -> Float -> Model -> Model
stepModel _ _ m = m
