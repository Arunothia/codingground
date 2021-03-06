------------------------------------------------------------------------------------------------------------
-- Written by M.Arunothia as a part of research internship under Prof.Harald, University of Melbourne.
------------------------------------------------------------------------------------------------------------
module PCE where
-- For SAT Solver
import AI.Surely 
import Picosat
-- For Priority Queue 
import Data.Set
import Data.Heap 
-- For find Function for List manipulation
import Data.List
-- For Printing Debug Statements
import System.IO.Unsafe

------------------------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS

-- fromJust Function

fromJust          :: Maybe a -> a
fromJust Nothing  = error "Error: fromJust detected Nothing" 
fromJust (Just x) = x

-- isJust Function

isJust 	:: Maybe a -> Bool
isJust Nothing 	= False
isJust (Just _) = True

--isSolution Function

isSolution :: Solution -> Bool
isSolution Unsatisfiable = False
isSolution (Solution _)  = True

--makePA Function

makePA :: [PAValue] -> PA
makePA x = PA $Just x

-- Print Function for debugging
-- To debug any variable just call $unsafePerformIO $debugPrint <variable>

debugPrint x = do
        	_ <- print x
        	return x

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

-- lessThanPA Function 

lessThanPA :: PA -> PA -> Bool
lessThanPA (PA Nothing)  _ 		= False
lessThanPA (PA (Just p)) (PA Nothing) 	= True
lessThanPA (PA (Just p)) (PA (Just q))	= ((length $Prelude.filter (==False) $zipWith isLessThan p q) == 0)

-- isLessThan Function

isLessThan :: PAValue -> PAValue -> Bool
isLessThan PAQuest PAQuest = True
isLessThan PAQuest _ 	   = False
isLessThan _ PAQuest 	   = True
isLessThan p q	     	   = if (p==q) then True else False

-- The following defines an order on Partial Assignments (The more undefined, higher the value)
-- The order should be more defined than just count of PAQuest, refer definition 

instance Ord PA where
  _ `compare` (PA Nothing) = GT
  (PA Nothing) `compare` _ = LT
  PA (Just p) `compare` PA (Just q)
	| (z /= EQ) = z
	| otherwise = zOrd
	where 	z = length(Prelude.filter (== PAQuest) p) `compare` length(Prelude.filter (== PAQuest) q)
	      	px = findIndices (== PATrue) p
		qx = findIndices (== PATrue) q
		py = Data.List.map (+ lth) $findIndices (== PAFalse) p
		qy = Data.List.map (+ lth) $findIndices (== PAFalse) q
		pz = Data.List.union px py
		qz = Data.List.union qx qy
		lth = length p
		zOrd = qz `compare` pz
------------------------------------------------------------------------------------------------------------

-- paTop is the fully unidentified partial assignment.
-- Partial Assignment in this code is defined by the data type Maybe [PAValue]

paTop :: Int -> PA
paTop 0 = error "[paTop:] Empty Vocabulary"
paTop n = PA $Just (replicate (fromIntegral n) PAQuest)

------------------------------------------------------------------------------------------------------------

-- assign Function defines a partial assignment when a literal is specified.
-- Its first integer input stands for 'n' - that denotes the vocabulary.
-- It takes an integer (negative for negated variables) and returns a partial assignment.
-- When the integer passed is zero (which should'nt be the case), the partial assignment returned marks all literals a contradiction.
 
assign :: Int -> Int -> PA
assign n 0 = error "[assign:] Literal value cannot be '0'"
assign 0 _ = error "[assign:] Empty Vocabulary"
assign n l
	| (abs(l) > n) = error "Invalid Literal"
	| (l > 0) = PA $Just(init(first) ++ [PATrue] ++ second) 
	| otherwise = PA $Just(init(first) ++ [PAFalse] ++ second)
	where  (first,second) = Data.List.splitAt (fromIntegral(abs(l))) (fromJust $unPA (paTop n))

------------------------------------------------------------------------------------------------------------
		  
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

------------------------------------------------------------------------------------------------------------

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
			paValueListOfC = Data.List.map (assignPAValueToL . fromIntegral) c
			assignPAValueToL l
				| (l > n || l < -n) = error "Literal value crossed n"
				| (l > 0) = p !! (l-1)
				| (l == 0) = error "[up:] Literal Value cannot be '0'"
				| otherwise = invertPAValue (p !! (abs(l)-1))
			invertPAValue v
				| (v == PAFalse) = PATrue
				| (v == PATrue) = PAFalse
				| otherwise = v

------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------

-- Though named as mus, all this function does is to convert PA' List to the correspoding encoding.


mus :: Int -> PA -> [Int]
mus _ (PA Nothing) = error "paPrimeList had a contradicting assignment"
mus n (PA (Just p))= Prelude.filter (/=0) (zipWith (*) (Data.List.map paValue p) (Data.List.map (* (-1)) [1..n]))

------------------------------------------------------------------------------------------------------------

-- pceAISurely is the function that will function as our Algorithm-1 (this function uses a not so good SAT Solver)
-- PCE - stands for Propagation Complete Encodings

-- Its arguments are
-- (1) An integer 'n', implying the vocabulary or variable set is X_1 to X_n
-- (2) A list of list of integers that will represent the CNF of E that is being computed (It will be E_0 at the start)
-- (3) A list of list of integers that will represent the CNF of E_ref (which is the CNF for which equisatisfiable formula is to be found)
-- (4) A maxHeap (priority queue) that is required by the algorithm for looping.

-- It returns 
-- (1) List of list of Ints representing the encoding that is equisatisfiable to E_ref and is propagation complete.

-- How List of List of Ints represent a CNF?
-- (1) The elements within the outer list are to be combined with an AND.
-- (2) The elements within the innner list are to be combined with an OR.
-- (3) Postive Int i represents the literal X_i and negative integer i represents the literal !X_i. 
-- (4) Int 0 is not included in the representation.

pceAISurely :: Int -> [Int] -> [[Int]] -> [[Int]] -> MaxHeap PA -> [[Int]]
pceAISurely n lst e eRef pq
	| (isEmpty pq) = e
	| otherwise = pceAISurely n lst eNew eRef pqNew						-- PQ.compact() not used yet	
	  where pqNew = Data.List.foldl' pushPQ (Data.Heap.drop 1 pq) paPrimeSatList		-- New Priority Queue after Loop.
		eNew = Data.List.union e (Data.List.map (mus n) paPrimeUnSatList)		-- New E after Loop.
		pushPQ queue p = Data.Heap.insert p queue			
		(paPrimeSatList, paPrimeUnSatList) = Data.List.partition isSat paPrimeList
		isSat p =  isJust $ AI.Surely.solve $sequence $Data.List.nub (Data.List.map (applyPA p) eRef)
		applyPA (PA Nothing) _  = error "paPrimeList had a contradicting assignment"
		applyPA _ [] = Just [-1,1]
		applyPA (PA (Just p)) c = isTrue (Prelude.filter (/=0) $Data.List.map (f p) c)
		f p l
		  |(l > 0) = if (p!!(l-1)) == PATrue then (n+1) else (if (p!!(l-1)) == PAFalse then 0 else l)
		  |(l == 0) = error "Literal Value cannot be '0'"
		  |otherwise = if (p!!(abs(l)-1)) == PATrue then 0 else (if (p!!(abs(l)-1)) == PAFalse then (n+1) else l)
		isTrue [] = Nothing
		isTrue x
		  |length(Prelude.filter (==(n+1)) x) > 0 = Just [1,-1]
		  |otherwise = Just x
		paPrimeList = Data.List.map paPrime loopList 				-- The pa' list 
		loopList = negEach $intersect lst $Data.List.map (+ 1) $Prelude.filter (>index) $findIndices (==PAQuest) paE
                                                                                -- The literal set for the loop.
                negEach xs = Data.List.foldr negate [] xs
                negate x y = x: -x : y
                paPrime l = paMeet pa (assign n l)                              -- pa' evaluation  
                paE = fromJust $ unPA $ gfpUP n e pa                            -- partial assignment returned from UP(E)(pa)
                index                                                           -- Should not assign variables before this
                  | (zlst == []) = -1                                           -- because otherwise there will be several 
                  | otherwise = last zlst                                       -- repetitions
                zlst = findIndices (/= PAQuest) $fromJust $unPA pa		
		pa = fromJust(viewHead pq) 					-- pa <- PQ.pop()
------------------------------------------------------------------------------------------------------------

-- pcePicosat is the function that will function as our Algorithm-1 (This function uses picosat Sat Solver)
-- PCE - stands for Propagation Complete Encodings

-- Its arguments are
-- (1) An integer 'n', implying the vocabulary or variable set is X_1 to X_n
-- (2) A list of list of integers that will represent the CNF of E that is being computed (It will be E_0 at the start)
-- (3) A list of list of integers that will represent the CNF of E_ref (which is the CNF for which equisatisfiable formula is to be found)
-- (4) A [PA] (priority queue) that is required by the algorithm for looping.

-- It returns 
-- (1) List of list of Ints representing the encoding that is equisatisfiable to E_ref and is propagation complete.

-- How List of List of Ints represent a CNF?
-- (1) The elements within the outer list are to be combined with an AND.
-- (2) The elements within the innner list are to be combined with an OR.
-- (3) Postive Int i represents the literal X_i and negative integer i represents the literal !X_i. 
-- (4) Int 0 is not included in the representation.

pcePicosat :: Int -> [Int] -> [[Int]] -> [[Int]] -> Set PA -> [[Int]]
pcePicosat n lst e eRef pq
	| (Data.Set.null pq)		= e
	| otherwise			= pcePicosat n lst eNew eRef pqNew              -- PQ.compact() not implemented yet
          where pqNew = Data.List.foldl' pushPQ queueList paPrimeSatList    		-- New Priority Queue after Loop.
		eNew = Data.List.union e $Data.List.map (mus n) paPrimeUnSatList	-- New E after Loop.
                pushPQ queue p = Data.Set.insert (gfpUP n eNew p) queue
		queueList = deleteMax pq
                (paPrimeSatList, paPrimeUnSatList) = Data.List.partition isSat paPrimeList
                isSat p =  isSolution $unsafePerformIO $ Picosat.solve 
					(if (solverQuery p) == Nothing then [[-1],[1]] else fromJust $solverQuery p)
		solverQuery p = sequence $Data.List.nub (Data.List.map (applyPA p) eRef)
                applyPA (PA Nothing) _  = error "paPrimeList had a contradicting assignment"
                applyPA _ [] = Just [-1,1]
                applyPA (PA (Just p)) c = isTrue (Prelude.filter (/=0) $Data.List.map (f p) c)
                f p l
                  |(l > 0) = if (p!!(l-1)) == PATrue then (n+1) else (if (p!!(l-1)) == PAFalse then 0 else l)
                  |(l == 0) = error "Literal Value cannot be '0'"
                  |otherwise = if (p!!(abs(l)-1)) == PATrue then 0 else (if (p!!(abs(l)-1)) == PAFalse then (n+1) else l)
                isTrue [] = Nothing
                isTrue x
                  |length(Prelude.filter (==(n+1)) x) > 0 = Just [1,-1]
                  |otherwise = Just x
                paPrimeList = Data.List.map paPrime loopList			-- The pa' list 
                loopList = negEach $intersect lst $Data.List.map (+ 1) $findIndices (==PAQuest) paE
                                                                                -- The literal set for the loop.
                negEach xs = Data.List.foldr negate [] xs
                negate x y = x: -x : y
                paPrime l = paMeet pa (assign n l)                              -- pa' evaluation  
                paE = fromJust $ unPA $gfpUP n e pa     			-- partial assignment returned from UP(E)(pa)
                pa = findMax pq        						-- pa <- PQ.pop()

------------------------------------------------------------------------------------------------------------

-- redundancyRemover Function removes the redundant clauses from the given encoding.
-- Note that, this function guarentees only a minimal encoding as the output and not the absolute least (as the outcome will depend on the processing order).
-- It takes an encoding and returns the encoding after removing redundant clauses, if any.

redundancyRemover :: [[Int]] -> [[Int]]
redundancyRemover e = redundancyRemoverHelper 0 (length e) n e
        where   n    = maximum (Data.List.map maximum eAbs)
                eAbs = Data.List.map (Data.List.map abs) e

redundancyRemoverHelper :: Int -> Int -> Int -> [[Int]] -> [[Int]]
redundancyRemoverHelper _ _ _ [] = []
redundancyRemoverHelper l m n e
        | (l<m)     = if ch then x else y
        | otherwise = e
                where   ch   = checkClause n rest c
                        x    = redundancyRemoverHelper l (m-1) n rest
                        y    = redundancyRemoverHelper (l+1) m n e
                        c    = e!!l
                        rest = Prelude.filter (/= c) e

------------------------------------------------------------------------------------------------------------

-- pceCheck Function checks whether any encoding is actually Propagation Complete by using author's encoding as correct (PC).
-- The function takes the value 'n' and 2 encodings as input and outputs True or False as desired.

pceCheck :: Int -> [[Int]] -> [[Int]] -> Bool
pceCheck _ [] _     = True
pceCheck n (c:cs) e
	| cond = pceCheck n cs e
	| otherwise = False
	where 	cond = checkClause n e c

-- checkClause Function is a helper function for pceCheck

checkClause :: Int -> [[Int]] -> [Int] -> Bool
checkClause _ e [] = True
checkClause n e c  = length(Prelude.filter (==True) $zipWith (==) oldPAList newPAList) == 0
	where 	newPAList = Data.List.map (fromJust . unPA . (gfpUP n e)) (Data.List.map makePA oldPAList)
		oldPAList = Data.List.map markLitQuest c
		markLitQuest x	   = Data.List.map (matchLit x) [1..n]
		matchLit x l
			| (abs(x) == l) = PAQuest
			| (index l == Nothing)= PAQuest
			| (c!!(fromJust(index l)) > 0)  = PAFalse
			| (c!!(fromJust(index l)) < 0)  = PATrue
			| otherwise = error "Error, 0 appeared"
		index l = getOrIndex (findIndex (==l) c) (findIndex (==(-l)) c)
		getOrIndex Nothing Nothing   = Nothing
		getOrIndex Nothing (Just a)  = Just a
		getOrIndex (Just a) Nothing  = Just a
		getOrIndex (Just a) (Just b) = error "Error,Literal found in both negated and abs form"
------------------------------------------------------------------------------------------------------------
