module Pathes where

latticeNodes = [[0, 0], [0, 0]] -- , [0, 0, 0]]
latticeVert = [0]

data NTree = Leaf a | Node a [NTree a]

-- data Grid = 

data Node   = Node (Start | End) (Corner | Center | Line)
data Corner = Bool | Bool
data Center = Bool | Bool | Bool | Bool
data Line   = Bool | Bool | Bool
 
size = 2
startxy = (0, 0)
endxy = (1, 1)

