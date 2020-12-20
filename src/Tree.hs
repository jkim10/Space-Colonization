module Tree(initialTree) where

type Leaves = [Leaf]
type Branches = [Branch]
data Leaf = Leaf Point | None
data Point = Point {x,y :: Float}
data Direction = N | W | S | E | NW | NE | SE | SW

data Branch = Branch {
                    position :: Point,
                    parent :: Leaf,
                    direction :: Direction
                    }

data Tree = Tree {
                 leaves :: Leaves,
                 root :: Branch,
                 branches  :: Branches,
                 max_dist :: Float,
                 min_dist :: Float
                 }

-- Convert Array of points to Leaves
pointsToLeaves :: [(Float, Float)] -> [Leaf]
pointsToLeaves arr = map (\(x,y) -> Leaf (Point x y)) arr


-- Initialize a tree
initialTree :: [(Float, Float)] -> Float -> Float -> Float -> Tree
initialTree arr size max min = Tree {
                                    leaves = pointsToLeaves arr,
                                    root = root_init,
                                    branches = [root_init],
                                    max_dist = max,
                                    min_dist = min
                                    }
                                where root_init = Branch {position=Point{x=(size / 2), y=0}, parent = None, direction = N}

calculateDistance :: Point -> Point -> Float
calculateDistance pos1 pos2 = let x' = (x pos1) - (x pos2)
                                  y' = (y pos1) - (y pos2)
                              in
                                  sqrt (x'*x' + y'*y')


nextBranch tree = 