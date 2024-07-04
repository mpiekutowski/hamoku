import           Control.Parallel.Strategies
import           Data.List
import           System.IO

-- Data ---------------------------------------------------------------------------------------------------------------
data Size = Size {sX :: [Integer], sY :: [Integer]}

data Maze = Maze {initPos :: Coord, lvl :: Lvl, mSize :: Size}

data Direction = R | U | L | D deriving Eq

data Tile = Border | Field | OO | XX deriving Eq

data Coord = C {x, y :: Integer} deriving (Show, Eq)

data State = S {
    position  :: Coord,
    direction :: Direction,
    ostones   :: [Coord],
    xstones   :: [Coord],
    turn      :: Color,
    lastBlue  :: Coord,
    lastRed   :: Coord,
    level     :: Lvl,
    size      :: Size
}

data Field = F {fPos :: Coord, fTile :: Tile} deriving Eq

data SSState world = StartScreen | Running world deriving Eq

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Screen)

data Event = KeyPress String deriving Eq

data WithUndo s = WithUndo s [s]

data Color = Blue | Red | Green | Black | Default deriving Eq

type Symbol = Char

type Screen = String

type Lvl = (Coord -> Tile)

type DrawFun = Integer -> Integer -> Char

type Picture = DrawFun -> DrawFun

blank :: a -> a
blank = id

(&) :: (b -> c) -> (a -> b) -> a -> c
(&) = (.)

-- Activity -----------------------------------------------------------------------------------------------------------
main :: IO ()
main = runActivity $ withStartScreen defaultActivity

runActivity :: Activity s -> IO ()
runActivity (Activity initial handle draw) = do
  prepareTerminal
  display $ draw initial
  stream <- getContents
  go initial stream
  where
    go state input = do
      (event, input') <- nextEvent input
      let state' = handle event state
      display $ draw state'
      if event == KeyPress "q" then exit else go state' input'

defaultActivity :: Activity State
defaultActivity = Activity initialState handleEvent drawState

prepareTerminal :: IO ()
prepareTerminal = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

display :: Screen -> IO ()
display screen = putStr $ "\ESCc" ++ "\ESC[?25l" ++ colored screen

nextEvent :: String -> IO (Event, String)
nextEvent ('\ESC' : '[' : c : cs) = return (KeyPress next, cs) where
  next
    | c == 'A'  = "w"
    | c == 'B'  = "s"
    | c == 'C'  = "d"
    | c == 'D'  = "a"
    | otherwise = ""
nextEvent (c : cs) = return (KeyPress [c], cs)
nextEvent [] = return (KeyPress "q", [])

exit :: IO ()
exit = do
  putStr "\ESC[?25h"
  return ()

-- Initial ------------------------------------------------------------------------------------------------------------
initialState :: State
initialState = initialStateOf $ head mazes

initialStateOf :: Maze -> State
initialStateOf maze =
  S (initPos maze) U [] [] Blue (C 0 0) (C 0 0) (lvl maze) (mSize maze)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U c = c {y = y c - 1}
adjacentCoord D c = c {y = y c + 1}
adjacentCoord R c = c {x = x c + 1}
adjacentCoord L c = c {x = x c - 1}

-- Handle -------------------------------------------------------------------------------------------------------------
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | isWinning s = s
  | key == " " = let s' = putStone s in
    if turn s' == Red then (putStoneAI s') {position = position s} else s
  | key == "w" = movePlayer s {direction = U}
  | key == "s" = movePlayer s {direction = D}
  | key == "d" = movePlayer s {direction = R}
  | key == "a" = movePlayer s {direction = L}
  | otherwise = s

isWinning :: State -> Bool
isWinning s =
  evaluateStone (lastBlue s) 5 (ostones s) (xstones s) /= 0 ||
  evaluateStone (lastRed s) 5 (xstones s) (ostones s) /= 0

stateLvl :: State -> Lvl
stateLvl s = addStones (ostones s) (xstones s) (level s)

addStones :: [Coord] -> [Coord] -> Lvl -> Lvl
addStones oStones xStones table coord
  | coord `elem` oStones = OO
  | coord `elem` xStones = XX
  | otherwise = table coord

movePlayer :: State -> State
movePlayer s
  | fTile targetField == Border = s
  | otherwise = s {position = fPos targetField}
  where
    targetField = getTargetField (stateLvl s) (direction s) (position s)

putStone :: State -> State
putStone s
  | turn s == Blue = putOStone s
  | otherwise = putXStone s

putOStone :: State -> State
putOStone s
  | elem (position s) (ostones s) || elem (position s) (xstones s) = s
  | otherwise = updateOStones (position s) s {turn = Red, lastBlue = position s}

putXStone :: State -> State
putXStone s
  | elem (position s) (ostones s) || elem (position s) (xstones s) = s
  | otherwise = updateXStones (position s) s {turn = Blue, lastRed = position s}

updateOStones :: Coord -> State -> State
updateOStones newCoord s =
  s {ostones = newCoord : ostones s}

updateXStones :: Coord -> State -> State
updateXStones newCoord s =
  s {xstones = newCoord : xstones s}

getTargetField :: Lvl -> Direction -> Coord -> Field
getTargetField lvl direction position =
  F targetCoord (lvl targetCoord)
  where
    targetCoord = adjacentCoord direction position

-- Consecutive -----------------------------------------------------------------
consecutiveInDirection :: Integer -> Coord -> [Coord] -> (Integer, Integer) -> [[Coord]]
consecutiveInDirection n coord coords dirs =
  filter (\sft -> intersect sft coords == sft) shiftees where
    shiftees = [shiftLeftConsecutive i dirs base | i <- [0..(n - 1)]]
    base = consecutiveBase n coord dirs

consecutiveBase :: Integer -> Coord -> (Integer, Integer) -> [Coord]
consecutiveBase n coord (dirX, dirY) =
  [C (x coord + i * dirX) (y coord + i * dirY) | i <- [0..(n - 1)]]

shiftLeftConsecutive :: Integer -> (Integer, Integer) -> [Coord] -> [Coord]
shiftLeftConsecutive n (dirX, dirY) = map (\c -> C (x c - n * dirX) (y c - n * dirY))

sameRow :: Coord -> [Coord] -> [Coord]
sameRow c = filter (\c' -> x c == x c')

sameCol :: Coord -> [Coord] -> [Coord]
sameCol c = filter (\c' -> y c == y c')

sameDiagDesc :: Coord -> [Coord] -> [Coord]
sameDiagDesc c = filter (\c' -> (x c - x c') == (y c - y c'))

sameDiagAsc :: Coord -> [Coord] -> [Coord]
sameDiagAsc c = filter (\c' -> (x c - x c') == (y c' - y c))

sameFuns :: [Coord -> [Coord] -> [Coord]]
sameFuns = [sameRow, sameCol, sameDiagDesc, sameDiagAsc]

dirs :: [(Integer, Integer)]
dirs = [(0, 1), (1, 0), (1, 1), (1, -1)]

-- Draw ---------------------------------------------------------------------------------------------------------------
drawState :: State -> Screen
drawState s = screenOf $
  if isWinning s
  then endScreen s (if evaluateStone (lastBlue s) 5 (ostones s) (xstones s) /= 0 then Blue else Red) & pictureOfState s
  else (drawAtCoord (position s) player) & pictureOfState s

pictureOfState :: State -> Picture
pictureOfState s = pictures ([drawAtCoord (C x y) (drawTile $ stateLvl s $ C x y) | x <- xR, y <- yR])
  where
    xR = sX $ size s
    yR = sY $ size s

drawAtCoord :: Coord -> Picture -> Picture
drawAtCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTile :: Tile -> Picture
drawTile Border = border
drawTile Field  = field
drawTile OO     = oo
drawTile XX     = xx

width, height :: Integer
width = 80
height = 23

widthRange, heightRange :: [Integer]
widthRange = [1..width]
heightRange = [1..height]

translated :: Integer -> Integer -> Picture -> Picture
translated x y picture f x' y' =
  picture (\x'' y'' -> f (x'' + x) (y'' + y)) (x' - x) (y' - y)

pictureOf :: Symbol -> Picture
pictureOf symbol _ 0 0 = symbol
pictureOf _ f x y      = f x y

screenOf :: Picture -> Screen
screenOf picture = [if x == width then '\n' else drawFun x y | y <- heightRange, x <- widthRange]
  where
    drawFun = picture (\_ _ -> ' ')

pictures :: [Picture] -> Picture
pictures list = foldl (&) blank list

lettering :: String -> Picture
lettering str = picture
  where
    letterPictures = map pictureOf str
    f' (pics, it) pic = (translated it 0 pic & pics, it + 1)
    (picture, _) = foldl f' (blank, 0) letterPictures

colored :: String -> String
colored str = str >>= coloredChar

-- Mazes --------------------------------------------------------------------------------------------------------------
mazes :: [Maze]
mazes = [maze1]

maze1 :: Maze
maze1 = Maze (C 8 8) lvl (Size [1..15] [1..15])
  where
    lvl (C x y)
      | x < 1 || y < 1 || x > 15 || y > 15  = Border
      | otherwise                           = Field


-- Assets -------------------------------------------------------------------------------------------------------------
border, field, oo, xx, player :: Picture
border = pictureOf ' '
field  = pictureOf '.'
oo     = pictureOf 'O'
xx     = pictureOf 'X'
player = pictureOf '@'

startScreen :: Picture
startScreen =
  centered hamokuStr
    & moveY 1 (centered playStr)
    & moveY 2 (centered exitStr)

finishedScreen :: State -> Color -> Picture
finishedScreen s c = centered wonStr where
  wonStr = if c == Blue then blueStr else redStr

endScreen :: State -> Color -> Picture
endScreen s c =
  finishedScreen s c
    & moveY 2 (centered overStr)
    & moveY 3 (centered exitStr)

centered :: String -> Picture
centered string = translated xCenter yCenter (lettering string)
  where
    xCenter = (width - (toInteger $ length string)) `div` 2
    yCenter = height `div` 2

moveY :: Integer -> Picture -> Picture
moveY y picture = translated 0 y picture

hamokuStr, playStr, exitStr, blueStr, redStr, overStr :: String
hamokuStr   = "Hamoku!"
playStr     = "[Press space to place stone]"
exitStr     = "[Press 'q' to exit]"
blueStr     = "Blue won!"
redStr      = "Red won!" 
overStr     = "Game over!"

coloredChar :: Char -> String
coloredChar c
  | c == '.'  = withColor Black c
  | c == 'O'  = withColor Blue c
  | c == 'X'  = withColor Red c
  | c == '@'  = withColor Green c
  | otherwise = [c]

withColor :: Color -> Char -> String
withColor color char = colorString color ++ [char] ++ colorString Default

colorString :: Color -> String
colorString Black   = "\ESC[90m"
colorString Blue    = "\ESC[96m"
colorString Red     = "\ESC[91m"
colorString Green   = "\ESC[92m"
colorString Default = "\ESC[0m"

-- Extensions ---------------------------------------------------------------------------------------------------------
withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity initial handle draw) =
  Activity initial' handle' draw'
  where
    initial' = StartScreen
    handle' (KeyPress " ") StartScreen = Running initial
    handle' _ StartScreen              = StartScreen
    handle' keypress (Running s)       = Running (handle keypress s)
    draw' StartScreen = screenOf startScreen
    draw' (Running s) = draw s

-- MinMax -------------------------------------------------------------------------------------------------------------
putStoneAI :: State -> State
putStoneAI s = putStone $ s {position = snd $ minMax s 1}

minMax :: State -> Integer -> (Integer, Coord)
minMax s 0 = bestMove s (zip moveValues goodMoves) where
  moveValues = map (\pos -> evaluatePosition $ putStone s {position = pos}) goodMoves
  goodMoves = filter (hasNeigh s) possibleMoves
  possibleMoves = (allMoves \\ ostones s) \\ xstones s

minMax s n = bestMove s (zip moveValues goodMoves) where
  moveValues = pMap (\pos -> fst $ minMax (putStone s {position = pos}) (n - 1)) goodMoves
  goodMoves = filter (hasNeigh s) possibleMoves
  possibleMoves = (allMoves \\ ostones s) \\ xstones s

hasNeigh :: State -> Coord -> Bool
hasNeigh s (C x y) =
  any (`elem` allStones) [C (x + xN) (y + yN) | xN <- [-1, 0, 1], yN <- [-1, 0, 1]] where
  allStones = ostones s ++ xstones s

allMoves :: [Coord]
allMoves = [C i j | i <- [1..15], j <- [1..15]]

bestMove :: State -> [(Integer, Coord)] -> (Integer, Coord)
bestMove s moveValues = head sorted where
  sorted = sortBy (\(mV, _) (mV', _) -> if turn s == Red then compare mV' mV else compare mV mV') moveValues

evaluatePosition :: State -> Integer
evaluatePosition s = xval - oval where
  xval = sum [evaluateConsLength i (xstones s) (ostones s) | i <- [1..5]]
  oval = sum [evaluateConsLength i (ostones s) (xstones s) | i <- [1..5]]

evaluateConsLength :: Integer -> [Coord] -> [Coord] -> Integer
evaluateConsLength n stones antistones =
   foldr (\c acc -> acc + evaluateStone c n stones antistones) 0 stones

evaluateStone :: Coord -> Integer -> [Coord] -> [Coord] -> Integer
evaluateStone pos n stones antistones =
  result where
  result = sum $ map (\r -> r ^ (2 * r)) ratings
  ratings =
     concatMap (\(dircons, dir) -> map (\dircon -> rateCons dircon dir antistones) dircons) (zip consecutives dirs)
  consecutives =
    [consecutiveInDirection n pos (f pos stones) d | (f, d) <- zip sameFuns dirs]

rateCons :: [Coord] -> (Integer, Integer) -> [Coord] -> Integer
rateCons cons dirs antistones
  | intersect [pCon, nCon] antistones == [pCon, nCon] = 0
  | pCon `elem` antistones = len - 1
  | nCon `elem` antistones = len - 1
  | otherwise = len
  where
      pCon = prevCon cons dirs
      nCon = nextCon cons dirs
      len = toInteger $ length cons

prevCon :: [Coord] -> (Integer, Integer) -> Coord
prevCon cons (dirX, dirY) = C (hX - dirX) (hY - dirY) where
  C hX hY = head cons

nextCon :: [Coord] -> (Integer, Integer) -> Coord
nextCon cons (dirX, dirY) = C (lX + dirX) (lY + dirY) where
  C lX lY = last cons
pMap f xs = map f xs `using` parList rseq
