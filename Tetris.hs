-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Data.List(transpose, deleteFirstsBy)
import Data.Maybe(isNothing, isJust)

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes


position :: Tetris -> Vector
position (Tetris (v,s) _ _ ) = v

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (vec, shape) well _ ) = wellTest && shapeTest -- Returns True if both are true,
  where
    shapeTest = propShape shape                                   -- checks that the shape follows the propShape function.
    wellTest = shapeSize well == wellSize                         -- Checks that the dimensions are equal to those of wellSize


--B05
addWalls :: Shape -> Shape
addWalls shape = S (hPadd ++ hWall ++ hPadd)                -- Applies the horisontal padding to the already vertically padded rows, then return as a shape
  where
    (sCol, _) = shapeSize(shape)                            -- Gets the size of the shape for dimensions.
    rowsOfShape = rows shape                                -- Gets the rows of the Shape to work with, i.e something to append onto.
    vPadd = addWalls' 1                                     -- Creates the Vertical padding.
    hPadd = [addWalls' (sCol + 2)]                          -- Creates the horisontal padding
    hWall = map (++ vPadd ) ((map (vPadd ++) rowsOfShape))  -- Map the wall to the left side of the shape, and then map that returning shape with a wall on the right side.


-- Creates a Row with n amount of Just Black to use as walls.
addWalls' :: Int -> [Maybe Colour]
addWalls' n = wall
  where
    wall = replicate n (Just Black)

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (vec ,shape) well remShapes) = addWalls combS  -- Add black walls around the new shape and return a new Shape
  where
    fallingS = place(vec, shape)                                   -- We shift the falling shape to the start position.
    combS = combine fallingS well                                 -- We combine i.e take the union of the well and the shape to put the shape inside the well


move :: Vector -> Tetris -> Tetris
move inVec (Tetris (vec, shape) well shapes ) = (Tetris (newVec, shape) well shapes ) -- returns the new Tetris after we've moved it by some vector
  where
    newVec = vAdd inVec vec  -- Creates the new vector by calling vAdd and sending the vector we recived as a parameter, and the current vector of the shape


tick :: Tetris -> Maybe (Int,Tetris)
tick t
  | collision t = dropNewPiece t --If a collision occurs, send this current state of the game to dropNewPiece
  | otherwise = Just(0, move (0, 1) t) -- If no colission, then use the updated shape, which is 1 col down/tick


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    numberList = [getNumber n | n <- rs]                -- Generate a random number based on the double
    shape1:supply = [allShapes !! n | n <- numberList]  -- From that random number, generate shapes.

-- Helper for startTetris to generate a random number
getNumber :: Double -> Int
getNumber double = floor(fromIntegral(length allShapes)* double)


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveRight t = Just(0, movePiece 1 t)
stepTetris MoveLeft t = Just(0, movePiece (-1) t)
stepTetris Rotate t = Just(0, rotatePiece t)


--C01
-- x Negativ = To far left
-- col + x > fst wellSize - To far to the right
-- row + y > snd wellSize -> To far down
-- overlaps shape(falling shape) well.
collision :: Tetris -> Bool
collision (Tetris (vector,shape) well remShapes) = or [x < 0, col + x > fst wellSize, row + y+1 > snd wellSize, overlapping]
  where
    (x, y) = vector                                     -- Gets the points from the vector for comparison
    (col, row) = shapeSize shape                        -- Gets the size from shape for comparison
    overlapping = overlaps (place ((x, (y+1)),shape)) well  -- Places the shape in correct spot, then check for overlaps between shape and well


--C03
movePiece :: Int -> Tetris -> Tetris
movePiece nMove tetris
  | collision movedPiece = tetris -- If a collision has occured, return to old state as we can't move it
  | otherwise = movedPiece        -- If no colission has occured, send back the moved piece.
    where
      movedPiece = move (nMove, 0) tetris -- Move the shape in either L or R direction,    movedPiece = move (nmove, 0)


--C04
rotate :: Tetris -> Tetris
rotate (Tetris (vector, shape) well shapes) = (Tetris (vector, (rotateShape shape))well shapes)

--C05
--If <0, to far left, and we need to fix
--if >10 to far to the right and we need to fix
adjust :: Tetris -> Tetris
adjust (Tetris (vec, shape) well shapes)
  | x < 0  = (Tetris (leftVector, shape) well shapes)  -- The shape is to far to the left and would overlap, therefore move the shape to the farthest left
  | (x + col) > 10 = (Tetris (rightVector, shape) well shapes) -- the shape is to far to the Right and would overlap, therefore move the shape to the farthest Right
  | otherwise =  (Tetris (vec, shape) well shapes)
  where
    (col, row)  = shapeSize shape -- Gets the sizes of the shape
    (x, y)      = vec             -- Gets the current place in horisontal position
    leftVector  = (0, y)          -- This would be the new position for the shape if to far to the left
    rightVector = ((10-col), y)   -- This would be the new Position for the shape if to far to the right

--C06
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision $ rotate t = t   -- If collision occurs when rotating t, use previous state
  | otherwise = rotate t     -- Otherwise rotate
  where
    --adjusted = adjust (rotate t) -- Adjust the rotated piece, use when the properly working


dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (vec,shape) w supply)
  | overlaps newShape w = Nothing
  | otherwise = Just(nCleared, nt)
    where
      newShape = (place (startPosition, head supply))
      newWell = combine (place(vec, shape)) w
      nt = (Tetris ((startPosition), head supply) clearedWell (drop 1 supply))
      (nCleared, clearedWell) = clearLines newWell

--C.09
-- Helper for Clearlines
isComplete :: Row -> Bool
isComplete row = all isJust row -- Filters out all the colours into a single list

-- Clears the lines and returns amount of rows cleared, and the new Shape
clearLines :: Shape -> (Int,Shape)
clearLines shape = (nRowsDone, shiftedNewShape)
  where
    doneRows = filter isComplete (rows shape) -- Gives a list of the completed rows
    nRowsDone = length doneRows               -- Finds out how many rows we've cleared
    newShape = deleteFirstsBy (==) (rows shape) doneRows  -- Deletes the first occurence of the element from the second list, in the first list. I.e we remove all the completed rows.
    shiftedNewShape = shiftShape (0, nRowsDone) (S newShape) -- Shifts the new shape down the amount of rows we've cleared, and add spaces over it
