{-# LANGUAGE OverloadedStrings #-}
module Day01 (someFunc) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

--input :: String
--input = unlines ["12", "14", "1969", "100756"]

someFunc :: IO ()
someFunc = parseInput >>= print .
		DL.foldl' (+) 0 .
		map fuelPerModule

-- hylomorphims
-- hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
fuelPerModule :: Int -> Int
fuelPerModule x = DL.foldl' (+) 0 $
	filter (>0) $
	DL.unfoldr unfolder x

parseInput :: IO [Int]
parseInput = map (read :: String -> Int) <$> (lines <$> getContents)

fuelForMass :: Int -> Int
fuelForMass x = x `div` 3 - 2

unfolder :: Int -> Maybe (Int, Int)
unfolder x
	| x <= 0 = Nothing
	| otherwise = Just (fuelForMass x, fuelForMass x)





