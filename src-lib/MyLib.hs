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
	print $ DL.foldl' (+) 0 $ map mapper $ map (read :: String -> Int) $ lines input
	where
	mapper :: Int -> Int
	mapper x = x `div` 3 - 2





