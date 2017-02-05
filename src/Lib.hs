module Lib where
import System.Random
import Data.List

showResults :: String -> IO ()
showResults str = putStrLn $ "Run completed with: " ++ str

-- Define a Game
data Game = NewGame {me :: Player, you :: Player}

hasWinner :: Game -> Bool
hasWinner (NewGame me you) = (isLoser me) || (isLoser you)

showWinner :: Game -> String
showWinner (NewGame me you)
  | (isLoser me) = "You win!!"
  | (isLoser you) = "Me win!!"
  | otherwise = "Unfinished Game"

shooterId :: Game -> PlayerID
shooterId (NewGame me you) = if (hasTurn me) then (title me) else (title you)

shoot :: Shot -> Game -> Game
shoot shot (NewGame (Person myTitle myBoard myTurn) (Person yourTitle yourBoard yourTurn))
  | (myTurn) && (myTitle == shooter shot) = NewGame (Person myTitle myBoard yourTurn) (Person yourTitle (recordA shot yourBoard) $ not yourTurn)
  | (yourTurn) && (yourTitle == shooter shot) = NewGame (Person myTitle (recordA shot myBoard) yourTurn) (Person yourTitle yourBoard $ not yourTurn)
  | otherwise = NewGame (Person myTitle myBoard myTurn) (Person yourTitle yourBoard yourTurn)

randomShooting ::  StdGen -> Game -> Game
randomShooting randomSeed game
  | hasWinner game = game
  | otherwise = let x = randomR(1,numRows) randomSeed
                    y = randomR(1,numCols) (snd x)
                    shooter = shooterId game
                    pos = Point (fst x) (fst y)
                    shot = NewShot pos shooter
                in randomShooting (snd y) $ shoot shot game

-- Define a player
data Player = Person { title :: PlayerID, board :: Board, hasTurn :: Bool}

takeA :: Shot -> Player -> Player
takeA shot player
  | shooter shot == title player = player
  | otherwise = player { board = recordA shot $ board player}

isLoser :: Player -> Bool
isLoser player = isLost $ board player

-- Define a board
numRows = 10 :: Int
numCols = 10 :: Int
listIndexes = [1 .. numCols * numRows] :: [Int]

data Board = NewBoard {listShips :: [Ship], hitCells :: [Int], missedCells :: [Int]}

isLost :: Board -> Bool
isLost board = all (\cell -> cell `elem` hitCells board) (listOccupiedCells board)

listOccupiedCells :: Board -> [Int]
listOccupiedCells board = concat (map (\sh -> occupiedCells sh) (listShips board) )

recordA :: Shot -> Board -> Board
recordA shot board =
    if (elem shotPosition  (listOccupiedCells board) ) then
      board {hitCells = shotPosition : (hitCells board)}
    else
      board {missedCells = shotPosition : (missedCells board)}
    where
      shotPosition = indexOf (this shot)

-- Define a ship

data ShipType = Carrier | Battleship | Submarine | Cruiser | Patrol

lengthOf :: ShipType -> Int
lengthOf Carrier = 5
lengthOf Battleship = 4
lengthOf Submarine = 3
lengthOf Cruiser = 2
lengthOf Patrol = 1

data Ship = NewShip {typeOf :: ShipType, headOf :: Position, isHorizontal :: Bool}

occupiedCells :: Ship -> [Int]
occupiedCells ship =
    if (isHorizontal ship) then
        [x .. (x + l -1)]
    else
        map (\int -> x + int * numCols) [0 .. l - 1]
    where
      x = indexOf (headOf ship)
      l = lengthOf $ typeOf ship

shipOnBoard :: Ship -> Bool
shipOnBoard ship = all (\a -> (a `elem` listIndexes)) (occupiedCells ship)

-- Define a shot
data Position = Point { row :: Int, col :: Int}

indexOf :: Position -> Int
indexOf (Point row col) = col + (row - 1) * numCols

isInBoard :: Position -> Bool
isInBoard point = (indexOf point) `elem` listIndexes

data PlayerID = You | Me deriving Eq

data Shot = NewShot {this :: Position, shooter :: PlayerID}