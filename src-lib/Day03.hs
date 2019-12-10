{- The idea is to build segments and then check their intersection -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day03 (someFunc) where

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

data Direction = East | North | West | South deriving Show

data Step = Step Direction Int deriving Show

data Segment
	-- X y0 yf
	= Vertical Int Int Int
	-- Y x0 xf
	| Horizontal Int Int Int
	deriving Show

input :: T.Text
input = "R8,U5,L5,D3\nU7,R6,D4,L4"

someFunc :: IO ()
someFunc = do
	print $ 
		solve . ((segments . extremes) *** (segments . extremes)) <$>
		AP.parseOnly inputParser input

solve :: ([Segment], [Segment]) -> [L.V2 Int]
solve (a, b) = concat $ intersection <$> a <*> b

intersection :: Segment -> Segment -> [L.V2 Int]
intersection (Vertical x y0 yf) (Vertical xx yy0 yyf)
	| x == xx = let
		(minY, maxY) = minMax y0 yf
		(minYY, maxYY) = minMax yy0 yyf
		lesserMax = min maxY maxYY
		greaterMin = max minY minYY
		in map (L.V2 x) [greaterMin..lesserMax]
	| otherwise = []
intersection (Horizontal x y0 yf) (Horizontal xx yy0 yyf)
	| x == xx = let
		(minY, maxY) = minMax y0 yf
		(minYY, maxYY) = minMax yy0 yyf
		lesserMax = min maxY maxYY
		greaterMin = max minY minYY
		in map (L.V2 x) [greaterMin..lesserMax]
	| otherwise = []
intersection (Horizontal x y0 yf) (Vertical y x0 xf)
	| (x0 <= x) && (x <= xf) && (y0 <= y) && (y <= yf) = [L.V2 x y]
	| otherwise = []
intersection v@(Vertical x y0 yf) h@(Horizontal y x0 xf) =
	intersection h v

-- *****
--    *****

minMax :: Ord a => a -> a -> (a, a)
minMax a b = (min a b, max a b)

segments :: NE.NonEmpty (L.V2 Int) -> [Segment]
segments (h :| []) = []
segments ((L.V2 x y) :| (h@(L.V2 xx yy) : ts))
	| x == xx = Vertical x y yy : segments (h:|ts)
	| y == yy = Horizontal y x xx : segments (h:|ts)

extremes :: [Step] -> NE.NonEmpty (L.V2 Int)
extremes [] = L.V2 0 0 :| []
extremes (s:ss) = NE.scanl scanner (L.V2 0 0) (s :| ss)
	where
	scanner :: L.V2 Int -> Step -> L.V2 Int
	scanner o s = o + stepVector s

stepVector :: Step -> L.V2 Int
stepVector (Step East dist) = L.V2 dist 0
stepVector (Step North dist) = L.V2 0 dist
stepVector (Step West dist) = L.V2 (-dist) 0
stepVector (Step South dist) = L.V2 0 (-dist)

inputParser :: AP.Parser ([Step], [Step])
inputParser = (,) <$>
	(AP.sepBy stepParser (AP.char ',')) <*>
	(do
		_ <- AP.char '\n'
		AP.sepBy stepParser (AP.char ','))

stepParser :: AP.Parser Step
stepParser = do
	dir <- AP.char 'R' <|> AP.char 'U' <|> AP.char 'L' <|> AP.char 'D'
	dist <- AP.many1 AP.digit
	return $ (case dir of
		'R' -> Step East
		'L' -> Step West
		'U' -> Step North
		'D' -> Step South) (read dist)
