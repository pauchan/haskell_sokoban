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
   | nextChar == '@' = moveCrate workerPos newWorkerPos nextChar map 
   | otherwise = insertWorker (eraseWorker map) newWorkerPos 

moveCrate :: (Int,Int) -> (Int,Int) -> Char -> [String] -> [String]
moveCrate = undefined

insertWorker :: [String] -> (Int, Int) -> [String]
insertWorker board position =
  let coords = zip  [0..] board
  in map (putWorkerInString position) coords

processChar :: Int -> (Int,Char) -> Char
processChar yPos char
  | fst char == yPos = 'o'
  | otherwise = snd char

processString :: String -> Int -> String
processString line yPos =
  let yCorrds = zip [0..] line
  in map (processChar yPos) yCorrds

putWorkerInString :: (Int, Int) -> (Int,String) -> String
putWorkerInString position line
  | fst line == fst position = processString (snd line) (snd position)
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
  | fst div == 0 = (snd origin, fst origin + snd div)
  | fst div == 1 = (snd origin + snd div, fst origin)
  | otherwise = (0,0)

getCoords :: [String] -> [(Maybe Int, Int)]
getCoords board =
  let xPosition = map (elemIndex 'o') board
      yPosition = [0..(length board)]
  in zip xPosition yPosition

findWorker :: [String] -> (Int, Int)
findWorker board =
  let results = getCoords board
      single = filter (isJust.fst) results
      s = head single
      a = fromMaybe 0 (fst s)
      b = snd s
  in (,) a b

decodeUserInput :: Char -> (Int, Int)
decodeUserInput x = case x of
	'w' -> (1,-1)
	'a' -> (0,-1)
	's' -> (1,1)
	'd' -> (0,1)
	_   -> (0,0)
