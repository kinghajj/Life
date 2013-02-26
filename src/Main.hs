--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (C) 2013 Sam Fredrickson
-- License     : BSD-style (see "LICENSE" file)
-- Maintainer  : Sam Fredrickson <kinghajj@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--------------------------------------------------------------------------------
module Main where

import Graphics.Gloss  (Display(InWindow), simulate, white)
import Life.Simulation (initialModel, stepModel)
import Life.View       (viewModel)

title :: String
title = "Life"

defaultWidth, defaultHeight, defaultX, defaultY :: Int
defaultWidth  = 800
defaultHeight = 600
defaultX      = 100
defaultY      = 100

main :: IO ()
main = simulate (InWindow title (defaultHeight, defaultWidth)
                                (defaultX, defaultY))
                white 60 initialModel viewModel stepModel
