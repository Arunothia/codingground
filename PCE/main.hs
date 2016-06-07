module Main where
import AI.Surely
import PCE

readInt :: IO [Int]
readInt = fmap (map read.words) getLine

readPAValue :: IO [PAValue]
readPAValue = fmap (map read.words) getLine

main = do
	putStrLn "Enter \n1: Debug paTop\n2: Debug assign\n3: Debug paMeet\n4: Debug up(Unit Propagation)\nANY OTHER NUMBER: No Debug"
	debugInput <- getLine
	let debug = read debugInput :: Int
	putStrLn "Enter value of Vocabulary 'n'"
	nInput <- getLine
	let n = read nInput :: Int
	if debug == 1 then print $ PCE.paTop n
	else if debug == 2 
		then do
			putStrLn "DEBUG 'assign'"
			putStrLn "Enter Literal value" 
		     	lInput <- getLine
		     	let l = read lInput :: Int
		     	print $ assign n l
	else if debug == 3
		then do
			putStrLn "DEBUG 'paMeet'"
			putStrLn "Enter 2 Literals (space seperated), (we use assign to check it)"
			[p,q] <- readInt
			print $ paMeet (assign n p) (assign n q)
	else if debug == 4
		then do
			putStrLn "DEBUG 'up'"
			putStrLn "Enter a clause (space seperated literals)"
			c <- readInt
			putStrLn "Enter a partial assignment ('n' Space seperated PAValues)" 
			p <- readPAValue
			print $ up n c (Just p)
	else return()
