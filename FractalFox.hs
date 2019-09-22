{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- module FractalFox where

import Data.Complex
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Grid

nextFox x = x # scale 25

foxCurves = map (snd . viewLoc . (`at` origin)) (iterate nextFox myCircle)
  where initialTrail = hrule 1

-- withPrevious combines each diagram in a list with a shadow of the previous one
withPrevious diagrams = zipWith (<>) diagrams (mempty : diagrams # opacity 0.2)

-- example = foxCurves
--                     -- # withPrevious
--                        # take 12
--                       -- # sameBoundingRect
--                     --    # rememberOrder
--                        # map (frame 0.1)
--                        -- # gridSnake
--                     --    # showOrder
--                        # lw ultraThin

myCircle :: Diagram B
myCircle = circle 1

-- circleSqT   = square 1 `atop` circle 1 # translate (r2 (0.5, 0.3))
-- circleSqHT  = square 1 ||| circle 1 # translate (r2 (0.5, 0.3))
-- circleSqHT2 = square 1 ||| circle 1 # translate (r2 (19.5, 0.3))

example = foxCurves # take 10 # gridSnake
-- example1 = hcat [circleSqT, strutX 1, circleSqHT, strutX 1, circleSqHT2]

-- main :: IO ()
main = mainWith (example :: Diagram B)
