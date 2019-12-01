{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

--input :: String
--input = unlines ["12", "14", "1969", "100756"]

someFunc :: IO ()
someFunc = do
	input <- getContents
	print $
		DL.foldl' (+) 0 $
		map fuelPerModule $ map (read :: String -> Int) $ lines input
	where
	fuelPerModule :: Int -> Int
	fuelPerModule x = fuelFuel x
	fuelFuel :: Int -> Int
	fuelFuel x = DL.foldl' (+) 0 $
		filter (>0) $
		DL.unfoldr unfolder x

fuelForMass :: Int -> Int
fuelForMass x = x `div` 3 - 2

unfolder :: Int -> Maybe (Int, Int)
unfolder x
	| x <= 0 = Nothing
	| otherwise = Just (fuelForMass x, fuelForMass x)





