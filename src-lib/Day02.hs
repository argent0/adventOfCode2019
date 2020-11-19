{- |
 - Module: Day02
 - Description: 1202 Program Alarm
 - Problem text at: problems/Day02
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Day02 (printResult) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as AP
import qualified Data.Array.MArray as AM
import qualified Data.Array.IArray as AI
import qualified Data.Array.Unboxed as AU
import qualified Data.Array.ST as AST
import qualified Control.Monad.ST as CMST
import Control.Lens

newtype MachineState = MachineState { getArray :: AU.UArray Int Int } deriving Show

machineOutput :: MachineState -> Int
machineOutput (MachineState a) = a AI.! 0

plusOpCode :: Int
plusOpCode = 1

multOpCode :: Int
multOpCode = 2

haltOpCode :: Int
haltOpCode = 99

--input :: T.Text
--input = "1,9,10,3,2,3,11,0,99,30,40,50"
--input = "1,0,0,0,99"
--input = "2,3,0,3,99"
--input = "2,4,4,5,99,0"
--input = "1,1,1,4,99,5,6,0,99"

run :: Int -> Int -> MachineState -> MachineState
run n v (MachineState ua) = MachineState $ CMST.runST stProg
	where
	stProg :: forall s . CMST.ST s (AU.UArray Int Int)
	stProg = do
		mem <- AST.thaw ua :: CMST.ST s (AST.STUArray s Int Int)
		(_, len) <- AM.getBounds mem
		AM.writeArray mem 1 n
		AM.writeArray mem 2 v
		loop mem len 0
		AST.freeze mem
	loop :: forall s . AST.STUArray s Int Int ->
		Int -> Int -> CMST.ST s ()
	loop mem maxIp ip 
		| ip > maxIp = return ()
		| otherwise = do 
			opCode <- AM.readArray mem ip
			if opCode == haltOpCode
				then return ()
				else do
					pos1 <- AM.readArray mem (ip + 1)
					pos2 <-  AM.readArray mem (ip + 2)
					resPos <- AM.readArray mem (ip + 3)
					arg1 <- AM.readArray mem pos1
					arg2 <- AM.readArray mem pos2
					if	| opCode == plusOpCode -> do
							AM.writeArray mem resPos (arg1 + arg2)
							loop mem maxIp (ip + 4)
						| opCode == multOpCode -> do
							AM.writeArray mem resPos (arg1 * arg2)
							loop mem maxIp (ip + 4)
						| otherwise -> return ()

data Result = Result
	{ _noun :: Int
	, _verb :: Int
	, _value :: Int } deriving Show

makeLenses ''Result

targetOutput :: Int
targetOutput = 19690720

printResult :: IO ()
printResult = do
	input <- AP.parseOnly inputParser <$> TIO.getContents
	case input of
		Left err -> putStrLn $ "ERROR: " ++ err
		Right inputOk ->
			print $
				filter (\res -> (res ^. value) == targetOutput) $
				(\n v ->
					Result n v $ machineOutput $ run n v $ MachineState inputOk) <$>
				[0..99] <*> [0..99]

inputParser :: AP.Parser (AU.UArray Int Int)
inputParser = do
	mem <- fmap read <$>
		AP.sepBy
		(AP.many1 AP.digit)
		(AP.char ',')
	return $ AI.listArray (0, length mem) mem
