-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Data.List(transpose)
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


well = padShapeTo (10,20)(S [[Nothing]])
shapes = [testShape, testShape3, testShape2, testShape, testShape3, testShape2, testShape]
testTetris = (Tetris (startPosition, testShape3) well shapes)

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
  | collision t = dropNewPiece t --(Just(0, t) -- Returns the old state, I.e stuck at bottom) 7C, If a collision occurs, send this current state of the game to dropNewPiece
  | otherwise = Just(0, newTet) -- If no colission, then use the updated shape, which is 1 col down/tick
  where
    newTet = move (0, 1) t      -- Basic operation for moving a shape down 1 col -- Default action for each gameTick

-- | The initial game state
--
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t
stepTetris MoveDown t = tick t
stepTetris MoveRight t = Just(0, movePiece 1 t)
stepTetris MoveLeft t = Just(0, movePiece (-1) t)
stepTetris Rotate t = Just(0, rotatePiece t)


-- x Negativ = To far left
-- col + x > fst wellSize - To far to the right
-- row + y > snd wellSize -> To far down
-- overlaps shape(falling shape) well
collision :: Tetris -> Bool
collision (Tetris (vector,shape) well remShapes) = or [x < 0, col + x > fst wellSize, row + y+1 > snd wellSize, overlaps shape well]
  where
    (x, y) = vector
    (col, row) = shapeSize shape

movePiece :: Int -> Tetris -> Tetris
movePiece nMove tetris
  | collision movedPiece = tetris -- If a collision has occured, return to old state as we can't move it
  | otherwise = movedPiece        -- If no colission has occured, send back the moved piece.
  where
    movedPiece = move (nMove, 0) tetris -- Move the shape in either L or R direction,

rotate :: Tetris -> Tetris
rotate (Tetris (vector, shape) well shapes) = (Tetris (vector, (rotateShape shape))well shapes)

rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision (rotate t) = t -- If collision occurs when rotating t, use previous state
  | otherwise = rotate t     -- Otherwise rotate

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (vec,shape) well shapes)
  | overlapping = Nothing         -- If Overlapping, then Nothing, as it's Game Over
  | otherwise = Just(0, newTet)   -- Otherwise update the game with the new state of the game.
  where
    newWell = combine shape well    -- Since the current state of the game means that this shape has collided with something, we merge this incoming shape with the well. - Before we start letting the new shape fall
    newShape = place(startPosition, head shapes) --Extract the first element from shapes - this is our new shape to play with - and place at startPos - Do this before checking overlaps, Otherwise we always get error as the shape's default pos is (0,0)
    newShapes = drop 1 shapes               -- Drops first element from the shapes, I.e we update the shapes, so we do not keep drawing the same piece
    overlapping = overlaps newShape well    -- checks if there's an overlap between the well(and pieces inside of it) and the new shape at the start pos
    newTet = (Tetris (startPosition, newShape) newWell newShapes) -- The new State of the Game
