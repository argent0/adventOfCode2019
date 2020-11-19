{-# LANGUAGE OverloadedStrings #-}
module Day01 (solution) where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.ByteString.Char8 (digit, endOfLine)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL

solution :: (Parser [Int], ([Int] -> Int, [Int] -> Int))
solution = (parseInput, (solver1, solver2))

-- | Generates the solution from the input
solver2 :: [Int] -> Int
solver2 = DL.foldl' (+) 0 . fmap fuelPerModule

solver1 :: [Int] -> Int
solver1 = DL.foldl' (+) 0 . fmap fuelForMass

-- | Computes the fuel needed by a module
--
-- It takes into account mass of the fuel
-- >>> fuelPerModule 14
-- 2
-- >>> fuelPerModule 100756
-- 50346
fuelPerModule :: Int -> Int
fuelPerModule = DL.foldl' (+) 0 .
	filter (>0) .
	DL.unfoldr unfolder
	where
	unfolder :: Int -> Maybe (Int, Int)
	unfolder x
		| x <= 0 = Nothing
		| otherwise = Just (fuelForMass x, fuelForMass x)

parseInput :: Parser [Int]
parseInput = 
	fmap (read :: String -> Int) . filter (not . null) <$>
		DAB.many' digit `DAB.sepBy` endOfLine

-- | The amount of fuel per unit of mass
--
-- >>> fuelForMass 12
-- 2
-- >>> fuelForMass 14
-- 2
-- >>> fuelForMass 1969
-- 654
-- >>> fuelForMass 100756
-- 33583
fuelForMass :: Int -> Int
fuelForMass x = x `div` 3 - 2
