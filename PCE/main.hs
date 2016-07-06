module Main where
import AI.Surely
import PCE
import Data.List.Split
import Data.Heap
import Data.Set
import Data.List

readInt :: IO [Int]
readInt = fmap (Prelude.map read. Prelude.words) getLine

readPAValue :: IO [PAValue]
readPAValue = fmap (Prelude.map read. Prelude.words) getLine

main = do
	putStrLn "Enter \n1: Debug paTop\n2: Debug assign\n3: Debug paMeet\n4: Debug up(Unit Propagation)\n5: Debug gfpUP\n6: Check Completeness by providing author's encoding"
	putStrLn "ANY OTHER NUMBER: PCE"
	debugInput <- getLine
	let debug = read debugInput :: Int
	putStrLn "Enter value of Vocabulary 'n' (Int n > 0)"
	nInput <- getLine
	let n = read nInput :: Int
	if debug == 1 then print $ PCE.paTop n
	else if debug == 2 
		then do
			putStrLn "*** DEBUG 'assign' ***"
			putStrLn "Enter Literal value" 
		     	lInput <- getLine
		     	let l = read lInput :: Int
		     	print $ assign n l
	else if debug == 3
		then do
			putStrLn "*** DEBUG 'paMeet' ***"
			putStrLn "Enter 2 Literals (space seperated), (we use assign to check it)"
			[p,q] <- readInt
			print $ paMeet (assign n p) (assign n q)
	else if debug == 4
		then do
			putStrLn "*** DEBUG 'up' ***"
			putStrLn "Enter a clause (space seperated literals)"
			c <- readInt
			putStrLn "Enter a partial assignment ('n' Space seperated PAValues)" 
			p <- readPAValue
			print $ up n c (PA (Just p))
	else if debug == 5
		then do
			putStrLn "*** DEBUG 'gfpUP' ***"
			putStrLn "Enter Set of Clauses (Each clause is space seperated literals) and the clauses are seperated by ','"
			inputSet <- getLine
    			let tmp = splitOneOf "," inputSet
    			let setC = (Prelude.map ((Prelude.map read).words) tmp)
			putStrLn "Enter a partial assignment ('n' Space seperated PAValues)"     
                        p <- readPAValue
			print $ gfpUP n setC (PA (Just p))
	else if debug == 6
		then do
			putStrLn "*** Check Completeness ***"
			putStrLn "Enter Author's Encoding (Each clause is space seperated) the clauses are seperated by ','"
                	inputE <- getLine
                	let tmp1 = splitOneOf "," inputE
                	let e = (Prelude.map ((Prelude.map read).words) tmp1)
                	let eCall = if e == [[]] then [] else e
                	putStrLn "Enter your encoding (Each clause is space seperated) the clauses are seperated by ','"
                	inputERef <- getLine
                	let tmp2 = splitOneOf "," inputERef
                	let eRef = (Prelude.map ((Prelude.map read).words) tmp2)
			print $ pceCheck n eCall eRef
			
	else do
		putStrLn "*** Computing Propagation Complete Encodings (PCE) ***"
		putStrLn "Enter the variables of interest (list of integers)"
		lst <- readInt
		putStrLn "Enter E_0 (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputE <- getLine
                let tmp1 = splitOneOf "," inputE
                let e = (Prelude.map ((Prelude.map read).words) tmp1)
		let eCall = if e == [[]] then [] else e
		putStrLn "Enter E_ref (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputERef <- getLine
                let tmp2 = splitOneOf "," inputERef
                let eRef = (Prelude.map ((Prelude.map read).words) tmp2)
		let pq = Data.Set.singleton (paTop n) 
		let answer = pcePicosat n lst eCall eRef pq
		_  <- print answer
		print $length answer
