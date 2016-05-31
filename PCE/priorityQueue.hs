{-
 -      ``Data/PriorityQueue''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -      
 -      the PriorityQueue kicks ass, if I do say so myself ;-)
 -      the |DefaultStateRef| class makes the choice of StateRef
 -      decidable, and the laxity of the StateRef classes' fundeps makes
 -      queues constructible in monads other than where they are intended
 -      to be used; eg:
 -      
 -         q <- newPriorityQueue show :: IO (PriorityQueue STM Integer)
 -      
 -      after which the whole interface to the queue is:
 -         enqueue (x :: Integer) q :: STM ()
 -         dequeue q :: STM Integer
 -      
 -      If the queue is being constructed in the same scope it is used,
 -      the full type of |newPriorityQueue f| can be inferred as well,
 -      as long as |f|'s target type is monomorphic.
 -      
 -}
{-# LANGUAGE
        ExistentialQuantification,
        MultiParamTypeClasses,
        FlexibleContexts,
        FlexibleInstances,
        CPP
  #-}

module Data.PriorityQueue
        ( Enqueue(..)
        , Dequeue(..)
        , DequeueWhere(..)
        , PeekQueue(..)
        , QueueSize(..)
        
        , PQ
        , emptyPQ
        , mkPriorityQueue
        , mkDefaultPriorityQueue
        
        , PriorityQueue
        , newPriorityQueue
        , newPriorityQueueBy
        
        ) where

import Data.Queue.Classes

import Control.Arrow ((&&&), (***))
import Data.StateRef
import Data.Ord.ReOrd
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, viewl, ViewL(..), (><), (<|), singleton, fromList)
import Data.List as List
import Data.Foldable as Foldable

-- |The "pure" type at the chewy center.
data PQ a = forall p. Ord p =>
        PQ { priorityFunc       :: a -> p
           , queue              :: M.Map p (Seq a)
           }

-- |A new empty 'PQ'
emptyPQ :: Ord p => (a -> p) -> PQ a
emptyPQ f = PQ f M.empty

-- |A priority queue usable in the monad 'm' with values of type 'a'
data PriorityQueue m a = 
    forall sr. ( ModifyRef sr m (PQ a) 
               ) => PriorityQueue sr

-- |Build a priority queue from a modifiable reference containing
--  a 'PQ'
mkPriorityQueue :: ModifyRef sr m (PQ a) => sr -> PriorityQueue m a
mkPriorityQueue = PriorityQueue

-- |Build a priority queue using an instance of the default modifiable 
--  reference for the requested monad and value type
mkDefaultPriorityQueue :: Ref m (PQ a) -> PriorityQueue m a
mkDefaultPriorityQueue = PriorityQueue

-- |Construct a new priority queue using the specified indexing function
newPriorityQueue :: 
        ( Monad m
        , HasRef m1
        , NewRef (Ref m1 (PQ a)) m (PQ a)
        , Ord p
        ) => (a -> p) -> m (PriorityQueue m1 a)
newPriorityQueue f = do
        pq <- newReference (emptyPQ f)
        
        return (mkDefaultPriorityQueue pq)

-- |Construct a new priority queue using a comparator function.  It is 
--  the user's responsibility to ensure that this function provides a
--  sensible order.
newPriorityQueueBy :: 
        ( Monad m
        , HasRef m1
        , NewRef (Ref m1 (PQ a)) m (PQ a)
        ) => (a -> a -> Ordering) -> m (PriorityQueue m1 a)
newPriorityQueueBy cmp = newPriorityQueue (ReOrd cmp)

instance Monad m => Enqueue (PriorityQueue m a) m a where
    enqueue (PriorityQueue pqRef) x = modifyReference pqRef $ \(PQ f pq) ->
        PQ f (M.insertWith (flip (><)) (f x) (singleton x) pq)
    
    -- the presumption here is that this is normally called for a bunch of
    -- elements of the same priority, so we prepare the input list by 
    -- grouping elements by priority.  In cases where the batch does have 
    -- large blocks of elements with the same priority, this will greatly 
    -- reduce the amount of work done by 'M.fromListWith'.  TODO: Test 
    -- whether (and when) this is worth the extra initial traversal.  Also
    -- check to make sure as much list fusion as I expect is actually 
    -- happening.
    enqueueBatch (PriorityQueue pqRef) xs = modifyReference pqRef $ \(PQ f pq) ->
        let prioritized = map (f &&& id) xs
            grouped = groupBy ((==) `on` fst) prioritized
            batches = map ((head *** fromList) . unzip) grouped
            newItems = M.fromListWith (flip (><)) batches
         in PQ f (M.unionWith (><) pq newItems)

instance Monad m => Dequeue (PriorityQueue m a) m a where
    dequeue (PriorityQueue pqRef) = atomicModifyReference pqRef $ \orig@(PQ f pq) ->
        case minViewWithKey pq of
            Nothing            -> (orig, Nothing)
            Just ((k,vs), pq') -> case viewl vs of
                EmptyL -> error "dequeue(PriorityQueue): internal inconsistency!"
                i :< is
                    | Seq.null is -> (PQ f pq', Just i)
                    | otherwise   -> (PQ f (M.insert k is pq'), Just i)

    dequeueBatch (PriorityQueue pqRef) = atomicModifyReference pqRef $ \orig@(PQ f pq) ->
        case M.minView pq of
            Nothing -> (orig, [])
            Just (xs, pq')
                | Seq.null xs -> error "dequeueBatch(PriorityQueue): internal inconsistency!"
                | otherwise   -> (PQ f pq', toList xs)

-- quick hack; there's probably a more efficient (and/or less ugly) way to do this
instance Monad m => DequeueWhere (PriorityQueue m a) m a where
    dequeueWhere (PriorityQueue pqRef) p = atomicModifyReference pqRef $ \orig@(PQ f pq) ->
        case List.break (Foldable.any p.snd) (M.toAscList pq) of
            (_, []) -> (orig, Nothing)
            (nonMatches, (k, firstMatch): rest) -> case extractFirstWhere p firstMatch of
                    (thing, otherThings)
                        | Seq.null otherThings ->
                            (PQ f (M.fromAscList (nonMatches ++ rest)), Just thing)
                        | otherwise -> 
                            (PQ f (M.fromAscList (nonMatches ++ (k, otherThings) : rest)), Just thing)

instance Monad m => PeekQueue (PriorityQueue m a) m a where
    peekQueue (PriorityQueue pqRef) = do
        PQ f pq <- readReference pqRef
        return [v | (k, vs) <- M.toAscList pq, v <- toList vs]

instance Monad m => QueueSize (PriorityQueue m a) m where
    queueSize (PriorityQueue pqRef) = do
        PQ f pq <- readReference pqRef
        return (M.fold (\xs t -> Seq.length xs + t) 0 pq)

-- |local version of minViewWithKey, because some versions of Data.Map
--  don't have it.
minViewWithKey :: M.Map k a -> Maybe ((k, a), M.Map k a)

#ifdef NoMinViewWithKey
minViewWithKey m = if M.null m
    then fail "empty map"
    else return (M.deleteFindMin m)
#else
minViewWithKey = M.minViewWithKey
#endif

breakl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
#ifdef NoBreakL
-- breakl p xs = (fromList ys, fromList zs)
--     where (ys, zs) = break p (toList xs)
breakl p xs = case viewl xs of
    EmptyL  -> (xs, xs)
    x :< xs' 
        | p x       -> (Seq.empty, xs)
        | otherwise -> let (ys, zs) = breakl p xs' in (x <| ys, zs)
#else
breakl = Seq.breakl
#endif

-- |'on' combinator (Data.Function doesn't always have it)
on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
op `on` f = \x y -> f x `op` f y

-- |given a Seq known to contain at least one item matching the predicate,
-- return the (first) matching item and the seq sans that element
extractFirstWhere :: (a -> Bool) -> Seq a -> (a, Seq a)
extractFirstWhere p xs = case breakl p xs of
    (noMatch, rest) -> case viewl rest of
        x :< rest -> (x, noMatch >< rest)
