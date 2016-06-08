module Main where
import AI.Surely
import PCE
import Data.List.Split
import Data.Heap

readInt :: IO [Int]
readInt = fmap (Prelude.map read. Prelude.words) getLine

readPAValue :: IO [PAValue]
readPAValue = fmap (Prelude.map read. Prelude.words) getLine

main = do
	putStrLn "Enter \n1: Debug paTop\n2: Debug assign\n3: Debug paMeet\n4: Debug up(Unit Propagation)\n5: Debug gfpUP\nANY OTHER NUMBER: PCE"
	debugInput <- getLine
	let debug = read debugInput :: Int
	putStrLn "Enter value of Vocabulary 'n'"
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
    			let setC = (Prelude.map ((map read).words) tmp)
			putStrLn "Enter a partial assignment ('n' Space seperated PAValues)"     
                        p <- readPAValue
			print $ gfpUP n setC (PA (Just p))
	else do
		putStrLn "*** Computing Propagation Complete Encodings (PCE) ***"
		putStrLn "Enter E_0 (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputE <- getLine
                let tmp1 = splitOneOf "," inputE
                let e = (Prelude.map ((map read).words) tmp1)
		putStrLn "Enter E_ref (Each clause is space seperated literals) and the clauses are seperated by ','"
                inputERef <- getLine
                let tmp2 = splitOneOf "," inputERef
                let eRef = (Prelude.map ((map read).words) tmp2)
		let pq = singleton (paTop n) :: MinHeap PA
		print $ pce n e eRef pq
