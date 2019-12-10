{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Day02 (printResult) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL
import qualified Data.Array.IArray as IA
import qualified Data.Attoparsec.Text as AP
import Data.Sequence (Seq(..))
import qualified Data.Sequence as DS
import Data.Functor.Foldable

import Control.Lens

data MachineState = MachineState
	{ _instructionPointer :: Int
	, _memory :: Seq Int } deriving Show
makeLenses ''MachineState

data Machine
	= Running MachineState
	| Halted deriving Show

initialState :: Seq Int -> MachineState
initialState = MachineState 0

plusOpCode :: Int
plusOpCode = 1

multOpCode :: Int
multOpCode = 2

haltOpCode :: Int
haltOpCode = 99

-- NOTE: Instead of this,
-- Use unfoldr to generate a list of states
--step :: MST.State MachineState ()
--step = do
--	ip <- use instructionPointer
--	mem <- use memory
--	let currentOpCode = mem DV.! ip
--	if currentOpCode == haltOpCode
--		then halted .= True
--		else do
--			let pos1 = mem DV.! (ip + 1)
--			let pos2 = mem DV.! (ip + 2)
--			let res = mem DV.! (ip + 3)
--			let arg1 = mem DV.! pos1
--			let arg2 = mem DV.! pos2
--			if currentOpCode == plusOpCode
--				then do
--					memory %= (DV.// [(res, arg1 + arg2)])
--					instructionPointer %= (+4)
--				else if currentOpCode == multOpCode
--					then do
--						memory %= (DV.// [(res, arg1 * arg2)])
--						instructionPointer %= (+4)
--					else halted .= True

step :: Machine -> Base [MachineState] Machine
step Halted = Nil
step (Running ms)
	| opCode == haltOpCode = Cons ms Halted
	| opCode == plusOpCode = Cons ms (Running (nextState (+)))
	| opCode == multOpCode = Cons ms (Running (nextState (*)))
	| otherwise = Nil
	where
	ip = ms ^. instructionPointer
	mem = ms ^. memory
	opCode = mem `DS.index` ip
	pos1 = mem `DS.index` (ip + 1)
	pos2 = mem `DS.index` (ip + 2)
	res = mem `DS.index` (ip + 3)
	arg1 = mem `DS.index` pos1
	arg2 = mem `DS.index` pos2
	nextState op = over instructionPointer (+4) $ over
					memory
					(DS.adjust (const (op arg1 arg2)) res)
					ms

input :: T.Text
--input = "1,9,10,3,2,3,11,0,99,30,40,50"
--input = "1,0,0,0,99"
--input = "2,3,0,3,99"
--input = "2,4,4,5,99,0"
input = "1,1,1,4,99,5,6,0,99"

setProgramAlarmState :: MachineState -> MachineState
setProgramAlarmState ms = over memory
	(DS.adjust (const 12) 1) $ over memory
	(DS.adjust (const 2) 2) ms

printResult :: IO ()
printResult = do
	input <- AP.parseOnly inputParser <$> TIO.getContents
	print (result <$> input)
	where
	result :: Seq Int -> [MachineState]
	result = ana step . Running . setProgramAlarmState . initialState

inputParser :: AP.Parser (Seq Int)
inputParser = DS.fromList . fmap read <$>
	AP.sepBy
	(AP.many1 AP.digit)
	(AP.char ',')

