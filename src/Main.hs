module Main where

main :: IO ()
main = do
	args <- getArgs
	handle <- openFile (head args) ReadMode
	contents <- hGetContents handle
	putStr contents
	hClose handle

-- the (Int, Int) tuple as a second parameter denotes
-- (horizontal/vertical orientation, back or forward)
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move = undefined

decodeUserInput :: Char -> (Int, Int)
decodeUserInput = undefined

applyMoveToMap :: [[Char]] -> (Int, Int) -> [[Char]]
applyMoveToMap = undefined
