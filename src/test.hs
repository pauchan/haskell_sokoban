extrapolate :: (Int,Int) -> (Int,Int) -> (Int, Int)
extrapolate previousPos nextPos
  | fst previousPos == fst nextPos = (fst nextPos, snd nextPos + (snd nextPos - snd previousPos))
  | otherwise = (fst nextPos + (fst nextPos - fst previousPos), snd nextPos) 

showPosition :: (Int, Int) -> String
showPosition pos = "first: " ++ show (fst pos) ++ " second: " ++ show (snd pos) 