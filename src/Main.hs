module Main where

import System.Environment
import System.IO
import Data.List


readMap :: String -> [String]
readMap x = lines x

main :: IO ()
main = do
	args <- getArgs
	contents <- readFile (head args)
	let level = readMap contents
	putStr (head level)
	command <- getChar
	let move = decodeUserInput command
	let l = applyMoveToMap level move
	putStr (head l)

	

applyMoveToMap :: [String] -> (Int, Int) -> [String]
applyMoveToMap = undefined

-- the (Int, Int) tuple as a second parameter denotes
-- (horizontal/vertical orientation (0/1), back or forward(-1/1))
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move = undefined

decodeUserInput :: Char -> (Int, Int)
decodeUserInput x = case x of
	'w' -> (1,-1)
	'a' -> (0,-1)
	's' -> (1,1)
	'd' -> (0,1)
	_   -> (0,0)
