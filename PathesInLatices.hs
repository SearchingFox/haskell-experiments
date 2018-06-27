module Pathes where

-- Arrays method
latticeNodes = [[0, 0], [0, 0]] -- , [0, 0, 0]]
latticeVert = [0]

-- Graph method 
data NTree = Leaf a | Node a [NTree a]
-- data Grid = 

data Corner = Bool | Bool
data Center = Bool | Bool | Bool | Bool
data Line   = Bool | Bool | Bool

-- Coordinates method
size = 2
startxy = (0, 0)
endxy = (1, 1)

