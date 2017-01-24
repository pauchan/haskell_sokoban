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

applyMoveToMap :: [String] -> (Int, Int) -> [String]
applyMoveToMap map coords =
	let workerPos = findWorker map
	    newWorkerPos = move workerPos coords
	    noWorkerMap = eraseWorker map
	in drawWorker noWorkerMap newWorkerPos

processChar :: Char -> Int -> Int -> Char
processChar char a b
  | a == b = 'o'
  | otherwise = char

putWorkerInString :: String -> (Int,Int) -> (Int,Int) -> String
putWorkerInString string a b
  | first a == first b = processChar 
  | otherwise = string

putWorkerInString :: String -> Int -> String
putWorkerInString line x =
  let xCoords = zip line [0..]
  in map (processChar.snd.x) xCoords

insertWorker :: [String] -> (Int, Int) -> [String]
insertWorker board position =
   --let yCoordBoard = zip [0..] board
  let coords = map (zip.[0..])
  in map (putWorkerInString.) coords



eraseWorker :: [String] -> [String]
eraseWorker board =
  let repl 'o' = ' '
      repl c = c
  in map (map repl) board

-- drawWorker :: [String] -> (Int, Int) -> [String]
-- drawWorker = undefined

-- the (Int, Int) tuple as a second parameter denotes
-- (horizontal/vertical orientation (0/1), back or forward(-1/1))
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move origin div
  | fst div == 0 = (fst origin + snd div, snd origin)
  | fst div == 1 = (fst origin, snd origin + snd div)
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
