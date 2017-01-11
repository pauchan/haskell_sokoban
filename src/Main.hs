module Main where

import System.Environment
import System.IO
import Data.List

readMap :: String -> [[Char]] -> [[Char]]
readMap x y = x : y

main :: IO ()
main = do
	args <- getArgs
	handle <- openFile (head args) ReadMode
	contents <- hGetContents handle
	let map = readMap contents []
	putStr (head map)
	hClose handle

applyMoveToMap :: [[Char]] -> (Int, Int) -> [[Char]]
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
