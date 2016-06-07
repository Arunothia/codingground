module Main where
import AI.Surely
import PCE

main = do
	putStrLn "Enter \n0: No Debug \n1: Debug paTop\n2: Debug assign"
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
	else return()
