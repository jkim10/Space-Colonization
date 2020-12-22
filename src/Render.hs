module Render where
 import Graphics.Gloss
 import Tree   
 
 drawPoint :: Leaf -> Picture
 drawPoint (Leaf (x,y) reached) = case reached of
                                       False -> Color red (Translate x y (ThickCircle 2 2))
                                       True -> Color green (Translate x y (ThickCircle 2 2))
 drawBranch :: Branch -> Picture
 drawBranch b = case (parent b) of
                    Empty -> Blank
                    otherwise -> let point = position b
                                     parent_point = position (parent b)
                                 in
                                     line [point,parent_point]

 treeAsPicture :: Tree -> Picture
 treeAsPicture tree = let branchPictures = map drawBranch (branches tree)
                          leafPictures = map drawPoint (leaves tree)
                      in
                          pictures (branchPictures ++ leafPictures)
