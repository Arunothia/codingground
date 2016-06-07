-----------------------------------------------------------------------------------------------------------
-- Written by M.Arunothia as a part of research internship under Prof.Harald, University of Melbourne.
-----------------------------------------------------------------------------------------------------------
module PCE where
-- For SAT Solver
import AI.Surely 
-- For Priority Queue 
import Data.Heap 
-- For fromJust Function :: Maybe a -> a
import Data.Maybe
-- For Function Fix
import Data.Function
-- For find Function for List manipulation
import Data.List

-----------------------------------------------------------------------------------------------------------

-- Data Type PAValue helps to define our partial assignment. It can be (True|False|Question|Contradiction).
-- The value 'contradiction' helps in identifying contradictions in the assignment of that variable.

data PAValue = PAFalse | PATrue | PAQuest | PAContra
		deriving (Eq, Read, Show)

-----------------------------------------------------------------------------------------------------------

-- paTop is the fully unidentified partial assignment.
-- Partial Assignment in this code is defined by the data type Maybe [PAValue]

paTop :: Int -> Maybe ([PAValue])
paTop 0 = Nothing
paTop n = Just (replicate n PAQuest)

-----------------------------------------------------------------------------------------------------------

-- assign Function defines a partial assignment when a literal is specified.
-- Its first integer input stands for 'n' - that denotes the vocabulary.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero (which should'nt be the case), the partial assignment returned marks all literals a contradiction.
 
assign :: Int -> Int -> Maybe [PAValue]
assign n 0 = Just (replicate n PAContra)
assign 0 _ = Nothing
assign n l
	| (l > 0) = Just(init(first) ++ [PATrue] ++ second) 
	| otherwise = Just(init(first) ++ [PAFalse] ++ second)
	where  (first,second) = Data.List.splitAt (abs(l)) (fromJust(paTop n))

-----------------------------------------------------------------------------------------------------------
		  
-- paMeet Function defines the 'meet' operator for combining two partial assignments.
-- If the two partial assignments lead to a contradiction, then we return Nothing (denoting contradiction).
-- Otherwise we return the unification of the two partial assignments.


paMeet :: Maybe [PAValue] -> Maybe [PAValue] -> Maybe [PAValue]
paMeet _ Nothing = Nothing
paMeet Nothing _ = Nothing
paMeet (Just p) (Just q) = if length(Prelude.filter (== PAContra) unifiedPA) > 0 then Nothing else Just unifiedPA  
	where unifiedPA = zipWith unifyPA p q
	      unifyPA a b
		| (a == b) = a
		| (a == PAQuest) = b
		| (b == PAQuest) = a
		| otherwise = PAContra

-----------------------------------------------------------------------------------------------------------

-- up(c) Function defines a unit propagation function that takes a partial assignment to another partial assignment.
-- It takes a list of integers (that represents a clause) as input.
-- It takes an integer 'n' - that is the vocabulary value.

up :: Int -> [Int] -> Maybe [PAValue] -> Maybe [PAValue]
up _ _ Nothing  = Nothing
up n c (Just p)
	| (((length c) -1 ==paFalseCount)&&(paQuestCount ==1)) = paMeet (assign n (c!!(fromJust(findIndex (==PAQuest) paValueListOfC)))) (Just p)
	| otherwise 					       = (Just p)
		where	paQuestCount = length (Prelude.filter (==PAQuest) paValueListOfC)
			paFalseCount = length (Prelude.filter (==PAFalse) paValueListOfC)
			paValueListOfC = map assignPAValueToL c
			assignPAValueToL l
				| (l > 0) = p !! (l-1)
				| (l == 0) = PAContra
				| otherwise = invertPAValue (p !! (abs(l)-1))
			invertPAValue v
				| (v == PAFalse) = PATrue
				| (v == PATrue) = PAFalse
				| otherwise = v

-----------------------------------------------------------------------------------------------------------

-- gfpUP Function - Greatest Fixed Point of applying up(c) for each clause in the encoding.
-- It takes the vocabulary value 'n', a set of clauses (List of list of integers) and a partial assignment as the input.
-- It outputs the GFP of unit propagation as mentioned in the paper

gfpUP :: Int -> [[Int]] -> Maybe [PAValue] -> Maybe [PAValue]
gfpUP _ _ Nothing     = Nothing
gfpUP n setC (Just p) = fix (\q -> paMeet (Just p) (bigPAMeet setC q))
	where bigPAMeet [] q = q
	      bigPAMeet (c:rest) q = bigPAMeet rest (up n c q)  

-----------------------------------------------------------------------------------------------------------

-- pce is the function that will function as our Algorithm-1
-- PCE - stands for Propagation Complete Encodings

-- Its arguments are
-- (1) An integer 'n', implying the vocabulary or variable set is X_1 to X_n
-- (2) A list of list of integers that will represent the CNF of E_0 (which is empty mostly)
-- (3) A list of list of integers that will represent the CNF of E_ref (which is the CNF for which equisatisfiable formula is to be found)

-- It returns 
-- (1) List of list of Integers representing the encoding that is equisatisfiable to E_ref and is propagation complete.

-- How List of List of Integers represent a CNF?
-- (1) The elements within the outer list are to be combined with an AND.
-- (2) The elements within the innner list are to be combined with an OR.
-- (3) Postive Integer i represents the literal X_i and negative integer i represents the literal !X_i. 
-- (4) Integer 0 is not included in the representation.

pce :: Int -> [[Int]] -> [[Int]] -> [[Int]]
pce n eIni eRef = eIni

-----------------------------------------------------------------------------------------------------------
