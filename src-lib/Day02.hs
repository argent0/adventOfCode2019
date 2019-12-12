{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Day02 (printResult) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as DL
import qualified Data.Attoparsec.Text as AP
import qualified Data.Array.MArray as AM
import qualified Data.Array.IArray as AI
import qualified Data.Array.Unboxed as AU
import qualified Data.STRef as DSTR
import qualified Data.Array.ST as AST
import qualified Control.Monad.ST as CMST
import qualified Control.Monad as CM
import Data.Functor.Foldable
import Control.Lens

newtype MachineState = MachineState { getArray :: AU.UArray Int Int } deriving Show

--initialState :: DV.Vector Int -> MachineState
--initialState = MachineState 0

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

--step :: Machine -> Base [MachineState] Machine
--step Halted = Nil
--step (Running ms)
--	| opCode == haltOpCode = Cons ms Halted
--	| opCode == plusOpCode = Cons ms (Running (nextState (+)))
--	| opCode == multOpCode = Cons ms (Running (nextState (*)))
--	| otherwise = Nil
--	where
--	ip = ms ^. instructionPointer
--	mem = ms ^. memory
--	opCode = mem `DS.index` ip
--	pos1 = mem `DS.index` (ip + 1)
--	pos2 = mem `DS.index` (ip + 2)
--	res = mem `DS.index` (ip + 3)
--	arg1 = mem `DS.index` pos1
--	arg2 = mem `DS.index` pos2
--	nextState op = over instructionPointer (+4) $ over
--					memory
--					(DS.adjust (const (op arg1 arg2)) res)
--					ms

input :: T.Text
--input = "1,9,10,3,2,3,11,0,99,30,40,50"
--input = "1,0,0,0,99"
--input = "2,3,0,3,99"
--input = "2,4,4,5,99,0"
input = "1,1,1,4,99,5,6,0,99"

run :: Int -> Int -> MachineState -> MachineState
run noun verb (MachineState ua) = MachineState $ CMST.runST stProg
	where
	stProg :: forall s . CMST.ST s (AU.UArray Int Int)
	stProg = do
		mem <- AST.thaw ua :: CMST.ST s (AST.STUArray s Int Int)
		(_, len) <- AM.getBounds mem
		-- TODO: Change this to use custom recursion in order
		-- to stop when the `haltOpCode` appears.
		AM.writeArray mem 1 noun
		AM.writeArray mem 2 verb
		loop mem len 0
		AST.freeze mem
	loop :: forall s . AST.STUArray s Int Int ->
		Int -> Int -> CMST.ST s ()
	loop mem maxIp ip 
		| ip > maxIp = return ()
		| otherwise = do 
			op <- AM.readArray mem ip
			if op == haltOpCode
				then return ()
				else do
					pos1 <- AM.readArray mem (ip + 1)
					pos2 <-  AM.readArray mem (ip + 2)
					resPos <- AM.readArray mem (ip + 3)
					arg1 <- AM.readArray mem pos1
					arg2 <- AM.readArray mem pos2
					if op == plusOpCode
						then do
							AM.writeArray mem resPos (arg1 + arg2)
							loop mem maxIp (ip + 4)
						else if op == multOpCode
							then do
								AM.writeArray mem resPos (arg1 * arg2)
								loop mem maxIp (ip + 4)
							else return ()

printResult :: IO ()
printResult = do
	input <- AP.parseOnly inputParser <$> TIO.getContents
	print $ run 12 2 . MachineState <$> input
	--where
	--result :: DV.Vector Int -> [MachineState]
	--result = undefined
		--ana step . Running . setProgramAlarmState . initialState

inputParser :: AP.Parser (AU.UArray Int Int)
inputParser = do
	mem <- fmap read <$>
		AP.sepBy
		(AP.many1 AP.digit)
		(AP.char ',')
	return $ AI.listArray (0, length mem) mem
