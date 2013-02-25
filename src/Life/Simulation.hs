{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Life.Simulation where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Simulate (ViewPort)
import Life.Model
import qualified Data.Set as Set

-- The simulation takes in the initial model and time delta as inputs.
-- Organisms use this initial model to make decisions.

type Config = (Model, Float)

-- State to keep track of while performing a step.

data SimState = SimState { _unprocessed :: Set.Set Object
                         , _removed     :: Set.Set Object
                         , _processed   :: Set.Set Object
                         }

makeLenses ''SimState

newtype Simulation a = Simulation {
    runSimulation :: ReaderT Config (StateT SimState IO) a
  } deriving (Functor, Applicative, Monad, MonadPlus,
              MonadReader Config, MonadState SimState)

process :: Object -> Simulation Object
process o = do
  beenRemoved <- Set.member o <$> use removed
  if beenRemoved || isFood o
    then return o
    else do (_, d) <- ask
            return $ (position +~ ((d,d) * (o ^. velocity))) o

simulation :: Simulation ()
simulation = do
  finished <- Set.null <$> use unprocessed
  if finished
    then return ()
    else do (o, unprocessed') <- Set.deleteFindMin <$> use unprocessed
            unprocessed .= unprocessed'
            beenRemoved <- Set.member o <$> use removed
            when (not beenRemoved) $ do
              o' <- process o
              processed' <- Set.insert o' <$> use processed
              processed .= processed'
            simulation

initialModel :: Model
initialModel = Set.fromList [
    Food 0 (100, 100) 20 10
  , Food 1 (-25, -75) 20 10
  , Organism 2 (0,0) (10, 10) 10 100 Male
  ]

stepModel :: ViewPort -> Float -> Model -> IO Model
stepModel _ d m =
  let cfg  = (m,d)
      st   = SimState m Set.empty Set.empty
      runS = runSimulation >>> runReaderT
  in do
    (_, st') <- runStateT (runS simulation cfg) st
    return $ Set.difference (st' ^. processed) (st' ^. removed)
