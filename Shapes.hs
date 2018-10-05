-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing, isJust)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

-- S constructor - []
data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions
testShape = allShapes !! 1
testShape2 = allShapes !! 2
testShape3 = allShapes !! 0
testRows = rows testShape
testRows2 = rows testShape2
testRows3 = rows testShape3
testRow = head testRows
testRow2 = head testRows2
errorShape = (S [[Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing]])


-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape(c, r) = S(replicate r c1)
  where
    c1 = emptyShape' c

--Helper funktion for getting the first row.
emptyShape' n = replicate n Nothing

-- ** A02
-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S rs) = (length(head(rs)), length(rs))

-- ** A03
-- | Count how many non-empty squares a shape contains
-- Kan användas isNothing, Filter
blockCount :: Shape -> Int
blockCount shape = length([c | c <- list, isJust c])
  where
     list = concat(rows shape)

-- * The Shape invariant
-- ** A04
propShape  :: Shape -> Bool
propShape (S rs) = colAndRow && isRec
  where
    (col, row) = shapeSize(S rs) -- Gathers the size of the shape
    colAndRow = and([col > 0]++[row > 0]) -- Checks that the row and col must be of length >0, otherwise there can't be a shape, as the smallest shape is 1x1
    intList = map length rs               -- Maps the length to each element of the rows to get an int list of the length of the rows.
    isRec = minimum(intList) == maximum(intList) -- Checks that the length of the rows are the same

-- * Test data generators

-- ** A05
-- | A random generator for colours
color_list = enumFrom Black     -- Creates a list of all the Colours

rColour :: Gen Colour
rColour =  elements color_list

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape shape = new_shape
  where
    rows' = rows shape
    transposed = transpose(reverse(rows'))  --
    new_shape = (S transposed)   --


-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (col, row) shape = finalShape
  where
    (tPaddedShape, _) = hPadding(col, row) shape      -- Padds Top, and return the new Shape
    (finalShape, _) = vPadding(col, row) tPaddedShape -- Padds the leftSide of the new Shape and returns a final Shape.


hPadding :: (Int, Int) -> Shape -> (Shape, Shape)
hPadding (col, row) shape = finalShapes
  where
    (currCol, currRow) = shapeSize(shape)        -- Gather the sizes that we can use for the Padding
    emptyRowToAppend = emptyShape(currCol, row)  -- Creates the padding we're gonna append.
    topPad = rows emptyRowToAppend ++ rows shape -- Appending the empty rows to the top of the Shape
    botPad = rows shape ++ rows emptyRowToAppend -- Appending the empty rows to the bottom of the Shape
    finalShapes = ((S topPad),(S botPad))        -- Returns the both padded shapes as a pair for pattern matching


vPadding :: (Int,Int) -> Shape -> (Shape, Shape)
vPadding (col, _) shape = finalShape
  where
    (_, curRow) = shapeSize(shape)                            -- Gather the sizes from Original Shape
    emptyRowToAppend = emptyShape(curRow, col)                -- Creates the padding we're gonna append.
    firstTransposedShape = transpose (rows shape)             -- Transposing the row so we can append a row onTop of it
    paddLeft = rows emptyRowToAppend ++ firstTransposedShape  -- Appending the rows on top of it
    paddRight = firstTransposedShape ++ rows emptyRowToAppend -- Appending the rows at the bottom of it
    transposeBackL = transpose paddLeft                       -- Transposing the shape back to it's previous form but Left Padded
    transposeBackR = transpose paddRight                      -- Transposing the shape back to it's previous form but Right Padded
    finalShape = ((S transposeBackL),(S transposeBackR))      -- Creates the final shape to return


-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (col, row) shape = finalShape
  where
    (_, bPaddedShape) = hPadding(col, row) shape      -- Padds bottom, and return the new Shape
    (_, finalShape) = vPadding(col, row) bPaddedShape -- Padds the Right of the new Shape and returns a final Shape.


-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (col, row) shape = padding
  where
    (curCol, curRow) = shapeSize shape                    -- Gets the current Size of the Shape
    padding = padShape ((col-curCol),(row-curRow)) shape  -- Pads to the given sizen, by adding the difference

-- * Comparing and combining shapes

-- Helper to more nicley get the first and second row when we're working with 2 shapes
twoRows :: Shape -> Shape -> ([Row],[Row])
twoRows s1 s2 = ((rows s1), (rows s2))


-- Maps isJust to the rows, to get a list of bools [[F,F],[T,T]]
-- then map and to these lists to get a combined answer ->[[F],[T]]
-- Finally evaluate the expression with an or, so that if either of them are True, then return True as they overlap
rowsOverlap :: Row -> Row -> Bool
rowsOverlap firstRow secondRow = or(map and [map isJust [firstRow !! i, secondRow !! i] | i <- [0..1]])


-- Takes 2 shapes, get their sizes, then padds them and return the rows so we can compare rows with eachother
padHelper :: Shape -> Shape -> ([Row],[Row])
padHelper s1 s2 = (s1Row, s2Row)
  where
    ((c1, r1):(c2, r2):_) = map shapeSize [s1, s2]      -- Uses map to extract the shapeSizes of both the shapes, and pattern match each value to a variable
    lengths = map maximum [[c1,c2],[r1,r2]]            --maps the maximum, to get the max length of row and col to know how to padShapeTo
    shapes = map (padShapeTo (lengths !! 0, lengths !! 1)) [s1, s2] -- We padd all the shapes, to make sure that we can compare our rows properly.
    (s1Row, s2Row) = twoRows (shapes !! 0) (shapes !! 1)            -- Extract the rows for the newly Padded Shapes.

clash :: Square -> Square -> Square
clash Nothing Nothing = Nothing
clash Nothing s       = s
clash s       Nothing = s
clash (Just c1) (Just c2) = Just Black

-- ** B01
-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps s1 s2 = or([rowsOverlap (s1Row !! i) (s2Row !! i) | i <- [0..(length s1Row)-1]]) -- Send each row to rowsOverlap and save the result, then evaluate the final list
  where
    (s1Row, s2Row) = padHelper s1 s2  -- Extract the rows for the newly Padded Shapes after they've been worked in padHelper

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
-- Creates a list of rows by Zipping with a given function, Then create a new shape with the combination of these rows.
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith function s1 s2 = (S [zipWith function (s1Row !! i) (s2Row !! i) | i <- [0..(length s1Row) -1]])
  where
    (s1Row, s2Row) = padHelper s1 s2 -- Extract the rows for the newly Padded Shapes after they've been worked in padHelper

-- ** B03
combine :: Shape -> Shape -> Shape
combine s1 s2 = zipShapeWith clash s1 s2    -- Combine calls the zipShapeWith, which calls shapeSize and padShapeWith
