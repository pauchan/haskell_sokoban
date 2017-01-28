module Main where

import System.Environment
import System.IO
import Data.List
import System.Process
import Data.Maybe



readMap :: String -> [String]
readMap x = lines x

main :: IO ()
main = do
	args <- getArgs
	contents <- readFile (head args)
	let level = readMap contents
	gameLoop level

renderMap :: [String] -> IO ()
renderMap x = do
	putStrLn (head x)
	if length x == 1 then return ()
	else  renderMap (tail x)
	

gameLoop :: [String] -> IO ()
gameLoop level = do 
	command <- getChar
	system "clear"
	let userInput = decodeUserInput command
	let l = applyMoveToMap level userInput
	renderMap l
	gameLoop l

charAtIndex :: [String] -> (Int, Int) -> Char
charAtIndex board position =
	let row = board !! fst position
	in row !! snd position

applyMoveToMap :: [String] -> (Int, Int) -> [String]
applyMoveToMap map coords =
	let workerPos = findWorker map
	    newWorkerPos = move workerPos coords
	    nextChar = charAtIndex map newWorkerPos
	in calculateNextMove workerPos newWorkerPos nextChar map 

calculateNextMove :: (Int, Int) -> (Int, Int) -> Char -> [String] -> [String]
calculateNextMove workerPos newWorkerPos nextChar map
   | nextChar == '#' = map
   | nextChar == '@' = moveCrate workerPos newWorkerPos map 
   | otherwise = insertWorker (eraseWorker map) newWorkerPos 

moveCrate :: (Int,Int) -> (Int,Int) -> [String] -> [String]
moveCrate previousPos nextPos map =
  let cratePos = extrapolate previousPos nextPos
  in renderCrate cratePos nextPos map

renderCrate :: (Int,Int) -> (Int,Int) -> [String] -> [String]
renderCrate cratePos nextPos map
  | charAtIndex map cratePos == '#' = map
  | otherwise = insertCrate (insertWorker (eraseWorker map) nextPos) cratePos

extrapolate :: (Int,Int) -> (Int,Int) -> (Int, Int)
extrapolate previousPos nextPos
  | fst previousPos == fst nextPos = (fst nextPos, snd nextPos + (snd nextPos - snd previousPos))
  | snd previousPos == snd nextPos = (fst nextPos + (fst nextPos - fst previousPos), snd nextPos) 
  | otherwise = error (show previousPos ++ show nextPos)

insertCrate :: [String] -> (Int, Int) -> [String]
insertCrate board position =
  let coords = zip  [0..] board
  in map (putInString position '@') coords

insertWorker :: [String] -> (Int, Int) -> [String]
insertWorker board position =
  let coords = zip  [0..] board
  in map (putInString position 'o') coords

processChar :: Int -> Char -> (Int,Char) -> Char
processChar yPos newChar char
  | fst char == yPos = newChar
  | otherwise = snd char

processString :: String -> Int -> Char -> String
processString line yPos char =
  let yCorrds = zip [0..] line
  in map (processChar yPos char) yCorrds

putInString :: (Int, Int) -> Char -> (Int,String) -> String
putInString position char line
  | fst line == fst position = processString (snd line) (snd position) char
  | otherwise = snd line

eraseWorker :: [String] -> [String]
eraseWorker board =
  let repl 'o' = ' '
      repl c = c
  in map (map repl) board

-- the (Int, Int) tuple as a second parameter denotes
-- (horizontal/vertical orientation (0/1), back or forward(-1/1))
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move origin div
  | fst div == 1 = (fst origin + snd div ,snd origin)
  | fst div == 0 = (fst origin, snd origin + snd div)
  | otherwise = (0,0)

getCoords :: [String] -> [(Int, Maybe Int)]
getCoords board =
  let xPosition = map (elemIndex 'o') board
      yPosition = [0..(length board)]
  in zip yPosition xPosition

findWorker :: [String] -> (Int, Int)
findWorker board =
  let results = getCoords board
      single = filter (isJust.snd) results
      s = head single
      a = fromMaybe 0 (snd s)
      b = fst s
  in (,) b a

decodeUserInput :: Char -> (Int, Int)
decodeUserInput x = case x of
	'w' -> (1,-1)
	'a' -> (0,-1)
	's' -> (1,1)
	'd' -> (0,1)
	_   -> (0,0)
