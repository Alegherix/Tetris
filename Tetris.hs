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
    fallingS = shiftShape vec shape                               -- We shift the falling shape to the start position.
    combS = combine fallingS well                                 -- We combine i.e take the union of the well and the shape to put the shape inside the well

move :: Vector -> Tetris -> Tetris
move inVec (Tetris (vec, shape) well shapes ) = (Tetris (newVec, shape) well shapes ) -- returns the new Tetris after we've moved it by some vector
  where
    newVec = vAdd inVec vec  -- Creates the new vector by calling vAdd and sending the vector we recived as a parameter, and the current vector of the shape


tick :: Tetris -> Maybe (Int,Tetris)
tick t = Just(0, move (0, 1) t) -- return Just and the new Tetris after we've moved it.

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t
stepTetris _ t = Just (0,t)
