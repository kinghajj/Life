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
  | isOrganism o = color (case (o ^. gender) of
                            Male   -> blue
                            Female -> magenta
                            None   -> black) $ circleSolid (o ^. size)
  | otherwise    = Text "?"

viewModel :: Model -> Picture
viewModel = Set.toList >>> map (twofer (^. position) viewObject) >>>
            map (uncurry $ uncurry Translate) >>> pictures
  where twofer f g a = (f a, g a)
