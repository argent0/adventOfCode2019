{- The idea is to generate all the passwords and filter the ones outside the
 - bounds -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day04 (someFunc) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL
import qualified Data.Array.IArray as IA
import qualified Data.Attoparsec.Text as AP
import qualified Data.Vector as DV
import qualified Control.Monad.State.Strict as MST
import Control.Applicative ((<|>))
import qualified Control.Applicative as CA
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Control.Arrow

import Control.Lens
import qualified Linear as L

data Pass = Pass Int Int [Int] [Int]

mkPass :: Int -> Int -> [Pass]
mkPass d p 
	| 0 <= d && d <= 9 = undefined
	| otherwise = error "Not a digit"

passRange :: Int -> Int -> [[Int]]
passRange s l
	| 0 == l = []
	| 0 < l && 0 <= s && s + l <= 9 = concatMap mapper [s .. s + l]
	| otherwise = error "Invalid range"
	where
	mapper :: Int -> [[Int]]
	mapper o = map (o:) $ passRange (o+1) (l-1)

input :: T.Text
input = "357253-892942"

someFunc :: IO ()
someFunc = print $
	AP.parseOnly inputParser input

inputParser :: AP.Parser (Integer, Integer)
inputParser = (,) <$>
	(read <$> AP.many1 AP.digit) <*>
	(read <$> do
		_ <- AP.char '-'
		AP.many1 AP.digit)
