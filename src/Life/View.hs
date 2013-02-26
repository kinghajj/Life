--------------------------------------------------------------------------------
-- |
-- Module      : Life.View
-- Copyright   : (C) 2013 Sam Fredrickson
-- License     : BSD-style (see "LICENSE" file)
-- Maintainer  : Sam Fredrickson <kinghajj@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Transform the model from Life.Mode into a Gloss Picture.
--------------------------------------------------------------------------------
module Life.View (viewModel) where

-- We'll use lots of items from these modules, so don't bother specifying all.

import Control.Arrow
import Control.Lens
import Graphics.Gloss
import Prelude hiding (id)
import Life.Model
import qualified Data.Set as Set

-- Transform the model into a picture.

viewObject :: Object -> Picture
viewObject o
  | isFood o     = color green $ circleSolid (o ^. size)
  | isOrganism o = let clr =
                         if (o ^. energy) <= 0
                          then black
                          else case (o ^. gender) of
                            Male   -> blue
                            Female -> magenta
                            None   -> black
                    in color clr $ circleSolid (o ^. size)
  | otherwise    = Text "?"

viewModel :: Model -> Picture
viewModel = Set.toList >>> map (twofer (^. position) viewObject) >>>
            map (uncurry $ uncurry Translate) >>> pictures
  where twofer f g a = (f a, g a)
