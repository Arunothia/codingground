module Main where
import AI.Surely
import PCE

readInt :: IO [Int]
readInt = fmap (map read.words) getLine

main = do
	putStrLn "Enter \n1: Debug paTop\n2: Debug assign\n3: Debug paMeet\nANY OTHER NUMBER: No Debug"
	debugInput <- getLine
	let debug = read debugInput :: Int
	putStrLn "Enter value of Vocabulary 'n'"
	nInput <- getLine
	let n = read nInput :: Int
	if debug == 1 then print $ PCE.paTop n
	else if debug == 2 
		then do
			putStrLn "Enter Literal value for checking assign" 
		     	lInput <- getLine
		     	let l = read lInput :: Int
		     	print $ assign n l
	else if debug == 3
		then do
			putStrLn "Enter 2 Literals (space seperated) for checking paMeet (we use assign to check it)"
			[p,q] <- readInt
			print $ paMeet (assign n p) (assign n q)
	else return()
