module Main where

import Graphics.Gloss (Display(InWindow), white)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Life.Simulation (initialModel, stepModel)
import Life.View (viewModel)

main :: IO ()
main = simulateIO (InWindow "Life" (800, 600) (100, 100)) white 30
                  initialModel (return . viewModel) stepModel
