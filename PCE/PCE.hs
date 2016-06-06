-----------------------------------------------------------------------------------------------------------
-- Written by M.Arunothia as a part of research internship under Prof.Harald, University of Melbourne.
-----------------------------------------------------------------------------------------------------------
module PCE where
-- For Looping Constructs
import Data.IORef 
-- For SAT Solver
import AI.Surely 
-- For Priority Queue 
import Data.Heap 
-- For Fix
import Data.Function 
-- For fromJust Function :: Maybe a -> a
import Data.Maybe
-- For find Function for List manipulation
import Data.List

-----------------------------------------------------------------------------------------------------------
-- Looping Constructs as given in http://www.xoltar.org/old_site/2003//sep/09/haskellLoops.html

-- Define 'foreach'

--foreach = flip mapM_

-- Define 'while'

--while test action = do

--  val <- test

--  if val then do {action;while test action}

--         else return ()

-- Some helpers for use with 'while':

--incr ref = modifyIORef ref (+1)

--test ref f = do { val <- readIORef ref; return (f val) }

-- Exercise them. Equivalent Python code:

-- for x in range(1,11): 

--     print x

-- i = 0

-- while i < 5:

--     print "Still running"

--     i += 1



-- main = do

--  foreach [1..10] print

--  ref <- newIORef 0

--  while (test ref (< 5))

--        (do 

--	  print "Still running!"

--          incr ref)
-----------------------------------------------------------------------------------------------------------

-- Data Type PAValue helps to define our partial assignment. It can be (True|False|Question), exactly the range of partial assignments.

data PAValue = PAFalse | PATrue | PAQuest
		deriving (Eq, Show)

-- An instance of Ord is to be written for Partial Assignments that will enable the functioning of priority queues. To be completed.

-- isQues and isFalse function do trivially what their name suggests

isQues :: PAValue -> Bool
isQues v = if (v == PAQuest) then True else False

isFalse :: PAValue -> Bool
isFalse v = if (v == PAFalse) then True else False

-- paTop Function takes any integer to a question. That is, the function is fully unidentified.

paTop :: Int -> PAValue
paTop v = PAQuest


-- assign(l) Function defines a partial assignment when a literal is specified.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero (which should'nt be the case), the partial assignment returned marks all literals false.
 
assign :: Int -> (Int -> PAValue) 
assign 0 = \x-> PATrue
assign l
	| (l>0) = \x -> if (x == l) then PATrue else PAQuest
	| otherwise = \x -> if (x == l) then PAFalse else PAQuest

-- paMeet Function defines the 'meet' operator for combining two partial assignments.
-- If the two partial assignments lead to a contradiction, then we return Nothing (denoting contradiction).
-- Otherwise we return the unification of the two partial assignments.

paMeet :: (Int -> PAValue) -> (Int -> PAValue) -> Maybe (Int -> PAValue)
paMeet p q = Nothing -- To be completed.

-- up(c) Function defines a unit propagation function that takes a partial assignment to another partial assignment.
-- It takes a list of integers (that represents a clause) as input.

up :: [Int] -> (Int -> PAValue) -> Maybe (Int -> PAValue)
up c = \p -> if ((length(Prelude.filter isQues (map p c)) == 1) && (length(Prelude.filter isFalse (map p c)) == length(c) - 1)) 
	     	then paMeet p (assign (fromJust (find (isQues.p) c)))
		else Just p

-- gfpUP Function - Greatest Fixed Point of applying up(c) for each clause in the encoding.
-- It takes a set of clauses (List of list of integers) and a partial assignment as the input.
-- It outputs the GFP of unit propagation as mentioned in the paper

gfpUP :: [[Int]] -> (Int -> PAValue) -> Maybe (Int -> PAValue)
gfpUP setC p = Nothing -- To be completed.

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
	--where 
	  --  heapPQ = Data.Heap.fromList [] :: MinHeap (Int -> PAValue)
	   -- ins = Data.Heap.insert paTop heapPQ
	   -- e = [] -- To be completed.
