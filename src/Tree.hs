module Tree where
import Data.List
import Control.Parallel.Strategies
type Leaves = [Leaf]
type Branches = [Branch]
data Leaf = Leaf Point Bool | None
type Point = (Float,Float)
data Branch = Empty | Branch {
                            position :: Point,
                            parent :: Branch,
                            direction :: Point -- Vector Representation of direction
                            }

data Tree = DONE | Tree {
                        leaves :: Leaves,
                        root :: Branch,
                        branches  :: Branches,
                        max_dist :: Float,
                        min_dist :: Float,
                        window_size :: Float,
                        detected :: Bool
                        }


{-
  Point Arithmetic Helpers
-}
add :: Point -> Point -> Point
add (x1, y1) (x2, y2) =
  let
    x = x1 + x2
    y = y1 + y2
  in (x, y)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) =
  let
    x = x1 - x2
    y = y1 - y2
  in (x, y)

normalize :: Floating b => (b, b) -> (b, b)
normalize (x,y) = let magnitude = sqrt ((x*x) + (y*y))
                  in
                    (x/magnitude, y/magnitude)

{-
  Tree,Branch,Leaf helpers
-}


-- Convert Array of points to Leaves
pointsToLeaves :: [(Float, Float)] -> [Leaf]
pointsToLeaves arr = parMap rseq (\(x,y) -> Leaf (x,y) False) arr

-- Check if Branch is 
notEmpty :: Branch -> Bool
notEmpty b = case b of
              Empty -> False
              otherwise -> True

-- Initialize a tree
initialTree :: [(Float, Float)] -> Float -> Float -> Float -> Tree
initialTree arr size max min = Tree {
                                    leaves = pointsToLeaves arr,
                                    root = root_init,
                                    branches = [root_init],
                                    max_dist = max,
                                    min_dist = min,
                                    window_size = size,
                                    detected = False
                                    }
                                where root_init = Branch {position=(0, -size/2), parent = Empty, direction = (0,1)}

addBranch :: Tree -> Branch -> Tree
addBranch tree branch = tree {branches= branch : (branches tree)}

addBranches :: Tree -> [Branch] -> Tree
addBranches tree b = tree {branches = b ++ (branches tree)}
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = let x' = x1 - x2
                               y' = y1 - y2
                           in
                               sqrt (x'*x' + y'*y')

detectLeaves branch leaves max_dist = any f leaves
                                      where f (Leaf (x,y) reached) = distance (x,y) (position branch) < max_dist



closestBranch :: Foldable t => Leaf -> t Branch -> Float -> Float -> (Leaf, Branch)
closestBranch (Leaf (x,y) _) branches min_dist max_dist = let closest = minimumBy f branches
                                                              dis = distance (position closest) (x,y)
                                                              direction = sub (x,y) (position closest)
                                                              normalized = normalize direction
                                                          in
                                                              if (dis > max_dist) then
                                                                ((Leaf (x,y) False), Empty)
                                                              else
                                                                if (dis <= min_dist) then
                                                                  ((Leaf (x,y) True),Branch {position=(add (position closest) normalized), parent=closest, direction = normalized})
                                                                else
                                                                  ((Leaf (x,y) False), Branch {position=(add (position closest) normalized), parent=closest, direction = normalized})
                                                          where f a b = compare (distance (position a) (x,y)) (distance (position b) (x,y))

-- calculateNewBranches closests = let grouped = groupBy position closests
      

step :: Tree -> Tree
step tree = let parent = head (branches tree)
            in
              case (detectLeaves parent (leaves tree) (max_dist tree)) of
                False -> addBranch tree (Branch {position=(add (position parent) (direction parent)), parent = parent, direction = (direction parent)})
                True -> tree {detected = True}

grow :: Tree -> Tree
grow tree = let unreached = filter (\(Leaf _ reached) -> not reached) (leaves tree)
                (newLeaves,closests) = unzip ((parMap rseq (\x -> closestBranch x (branches tree) (min_dist tree) (max_dist tree)) unreached))
                newBranches = filter notEmpty closests
            in
                case newBranches of
                  [] -> DONE
                  otherwise -> addBranches (tree {leaves = newLeaves}) newBranches

nextBranch :: p1 -> p2 -> Tree -> Tree
nextBranch _ _ tree = case (detected tree) of
                          False -> step tree
                          True -> grow tree
                          