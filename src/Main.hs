module Main where

import Graphics.Gloss (Display(InWindow), simulate, white)
import Life.Simulation (initialModel, stepModel)
import Life.View (viewModel)

main :: IO ()
main = simulate (InWindow "Life" (800, 600) (100, 100)) white 60
                initialModel viewModel stepModel
