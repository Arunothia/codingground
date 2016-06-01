module PCE where
import AI.Surely
import Data.Heap
import Data.Function

-- Data Type PAValue helps to define our partial assignment. It can be (True|False|Question), exactly the range of partial assignments.

data PAValue = PAFalse | PATrue | PAQuest

-- Assign(l) Function that defines a partial assignment for a function.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero (which should'nt be the case), the partial assignment returned marks all literals false.
 
assign :: Int -> (Int -> PAValue) 
assign 0 = \x->True
assign l
	| (l>0) = \x -> if (x == l) then PATrue else \x -> PAQuest
	| otherwise = \x -> if (x == l) then PAFalse else PAQuest


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

