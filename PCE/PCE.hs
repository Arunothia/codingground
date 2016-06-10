------------------------------------------------------------------------------------------------------------
-- Written by M.Arunothia as a part of research internship under Prof.Harald, University of Melbourne.
------------------------------------------------------------------------------------------------------------
module PCE where
-- For SAT Solver
import AI.Surely 
-- For Priority Queue 
import Data.Heap 
-- For find Function for List manipulation
import Data.List

-----------------------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS

-- fromJust Function

fromJust          :: Maybe a -> a
fromJust Nothing  = error "Error: fromJust detected Nothing" 
fromJust (Just x) = x

-- isJust Function

isJust 	:: Maybe a -> Bool
isJust Nothing 	= False
isJust (Just _) = True

------------------------------------------------------------------------------------------------------------

-- Data Type PAValue helps to define our partial assignment. It can be (True|False|Question).

data PAValue = PAFalse | PATrue | PAQuest
		deriving (Eq, Read, Show)

paValue :: PAValue -> Int
paValue PAFalse = -1
paValue PATrue 	= 1
paValue PAQuest = 0

-- PA - The Type of Partial Assignment we will be dealing with

newtype PA = PA (Maybe([PAValue]))
		deriving (Eq, Read, Show)

-- To undo PA constructor use unPA Function

unPA :: PA -> Maybe([PAValue])
unPA (PA v) = v

-- The following defines an order on Partial Assignments (The more undefined, higher the value) 

instance Ord PA where
  _ `compare` (PA Nothing) = GT
  (PA Nothing) `compare` _ = LT
  PA (Just p) `compare` PA (Just q) = length(Prelude.filter (== PAQuest) p) `compare` length(Prelude.filter (== PAQuest) q)

------------------------------------------------------------------------------------------------------------

-- paTop is the fully unidentified partial assignment.
-- Partial Assignment in this code is defined by the data type Maybe [PAValue]

paTop :: Int -> PA
paTop 0 = error "Empty Vocabulary"
paTop n = PA $Just (replicate n PAQuest)

-----------------------------------------------------------------------------------------------------------

-- assign Function defines a partial assignment when a literal is specified.
-- Its first integer input stands for 'n' - that denotes the vocabulary.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero (which should'nt be the case), the partial assignment returned marks all literals a contradiction.
 
assign :: Int -> Int -> PA
assign n 0 = error "Literal value cannot be '0'"
assign 0 _ = error "Empty Vocabulary"
assign n l
	| (abs(l) > n) = error "Invalid Literal"
	| (l > 0) = PA $Just(init(first) ++ [PATrue] ++ second) 
	| otherwise = PA $Just(init(first) ++ [PAFalse] ++ second)
	where  (first,second) = Data.List.splitAt (abs(l)) (fromJust $unPA (paTop n))

-----------------------------------------------------------------------------------------------------------
		  
-- paMeet Function defines the 'meet' operator for combining two partial assignments.
-- If the two partial assignments lead to a contradiction, then we return Nothing (denoting contradiction).
-- Otherwise we return the unification of the two partial assignments.


paMeet :: PA -> PA -> PA
paMeet _ (PA Nothing) = PA Nothing
paMeet (PA Nothing) _ = PA Nothing
paMeet (PA (Just p)) (PA (Just q)) = PA $ sequence unifiedPA
	where unifiedPA = zipWith unifyPA p q
	      unifyPA a b
		| (a == b) = (Just a)
		| (a == PAQuest) = (Just b)
		| (b == PAQuest) = (Just a)
		| otherwise = Nothing

-----------------------------------------------------------------------------------------------------------

-- up(c) Function defines a unit propagation function that takes a partial assignment to another partial assignment.
-- It takes a list of integers (that represents a clause) as input.
-- It takes an integer 'n' - that is the vocabulary value.

up :: Int -> [Int] -> PA -> PA
up _ _ (PA Nothing)  = (PA Nothing)
up n c (PA (Just p))
	| (((length c) -1 ==paFalseCount)&&(paQuestCount ==1)) 
						= paMeet (assign n (c!!(fromJust(findIndex (==PAQuest) paValueListOfC)))) (PA (Just p))
	| otherwise 					       = (PA (Just p))
		where	paQuestCount = length (Prelude.filter (==PAQuest) paValueListOfC)
			paFalseCount = length (Prelude.filter (==PAFalse) paValueListOfC)
			paValueListOfC = map assignPAValueToL c
			assignPAValueToL l
				| (l > 0) = p !! (l-1)
				| (l == 0) = error "Literal Value cannot be '0'"
				| otherwise = invertPAValue (p !! (abs(l)-1))
			invertPAValue v
				| (v == PAFalse) = PATrue
				| (v == PATrue) = PAFalse
				| otherwise = v

-----------------------------------------------------------------------------------------------------------

-- gfpUP Function - Greatest Fixed Point of applying up(c) for each clause in the encoding.
-- It takes the vocabulary value 'n', a set of clauses (List of list of integers) and a partial assignment as the input.
-- It outputs the GFP of unit propagation as mentioned in the paper.

gfpUP :: Int -> [[Int]] -> PA -> PA
gfpUP n setC p = greatestFP p
	where greatestFP q
		| (q == (bigPAMeet setC q)) = q
		| otherwise = greatestFP (bigPAMeet setC q) 
	      bigPAMeet [] q = q
	      bigPAMeet (c:rest) q = bigPAMeet rest (up n c q)  

-----------------------------------------------------------------------------------------------------------

-- pce is the function that will function as our Algorithm-1
-- PCE - stands for Propagation Complete Encodings

-- Its arguments are
-- (1) An integer 'n', implying the vocabulary or variable set is X_1 to X_n
-- (2) A list of list of integers that will represent the CNF of E that is being computed (It will be E_0 at the start)
-- (3) A list of list of integers that will represent the CNF of E_ref (which is the CNF for which equisatisfiable formula is to be found)
-- (4) A minHeap (priority queue) that is required by the algorithm for looping.

-- It returns 
-- (1) List of list of Integers representing the encoding that is equisatisfiable to E_ref and is propagation complete.

-- How List of List of Integers represent a CNF?
-- (1) The elements within the outer list are to be combined with an AND.
-- (2) The elements within the innner list are to be combined with an OR.
-- (3) Postive Integer i represents the literal X_i and negative integer i represents the literal !X_i. 
-- (4) Integer 0 is not included in the representation.

pce :: Int -> [[Int]] -> [[Int]] -> MinHeap PA -> [[Int]]
pce n e eRef pq
	| (isEmpty pq) = e
	| otherwise = pce n eNew eRef pqNew
		where 	pqNew = foldl' pushPQ empty paPrimeSatList		-- New Priority Queue after Loop.
			eNew = Data.List.union e (map mus paPrimeUnSatList)	-- New E after Loop.
			pushPQ queue p = Data.Heap.insert p queue		-- MUS (Unsatisfiable Core - not minimal)
			mus (PA Nothing) = error "paPrimeList had a contradicting assignment"	
			mus (PA (Just p))= Prelude.filter (/=0) (zipWith (*) (map paValue p) (map (* (-1)) [1..n]) )
			(paPrimeSatList, paPrimeUnSatList) = Data.List.partition isSat paPrimeList
			isSat p = isJust(AI.Surely.solve $applyPA p)
			applyPA (PA Nothing) = error "paPrimeList had a contradicting assignment"
			applyPA (PA (Just p))= [] -- To be completed!!!!!
			paPrimeList = map paPrime loopList 			-- The pa' list 
			loopList = negEach $findIndices (==PAQuest) paE 	-- The literal set for the loop.
			negEach xs = foldr negate [] xs  	
    			negate x y = x : -x : y 			
			paPrime l = paMeet pa (assign n l) 			-- pa' evaluation  
			paE = fromJust $ unPA $ gfpUP n e pa 			-- partial assignment returned from UP(E)(pa)
			pa = fromJust(viewHead pq) 				-- pa <- PQ.pop()

-----------------------------------------------------------------------------------------------------------
