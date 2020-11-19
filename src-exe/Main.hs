module Main where

import qualified Day01

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as DAB

runSolution :: Show b => (Parser a, (a -> b, a -> b)) -> IO ()
runSolution (parser, (solver1, solver2)) = do
	contents <- BS.getContents
	let parseResult = DAB.parseOnly parser contents
	case parseResult of
		Left err -> putStrLn err
		Right input -> do
			print $ solver1 input
			print $ solver2 input

main :: IO ()
main = runSolution Day01.solution
