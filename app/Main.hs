module Main where
import System.Random
import Lib

main :: IO ()
main = do
  let allShipTypes = [Carrier , Battleship , Submarine , Cruiser , Patrol]
  let horizontalShips = map (\shipType -> NewShip shipType (Point (lengthOf shipType) 1) True) allShipTypes
  let verticalShips = map (\shipType -> NewShip shipType (Point 1 (lengthOf shipType)) False) allShipTypes
  let me = Person Me (NewBoard horizontalShips [] []) True
  let you = Person You (NewBoard verticalShips [] []) False
  let game = NewGame me you
  g <- newStdGen
  let thisGame = randomShooting g game
  -- Print out the winner
  showResults $ showWinner thisGame