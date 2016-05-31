{-
 -      ``Data/Queue/Classes''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        MultiParamTypeClasses,
        FunctionalDependencies
  #-}

module Data.Queue.Classes where

-- |Construct a new FIFO queue.
class Monad m => NewFifo q m where
        newFifo :: m q

-- |A type class carrying an altered set of functional dependencies used to
--  constrain queues when the type of the queue never escapes far enough for
--  a more deliberate choice to be made.
class Monad m => DefaultFifo q m a | q -> a, m a -> q

class Monad m => Enqueue q m a | q -> a where
        -- |Put an item into a queue.  May block while trying to do so.
        --  No constraint is placed on the behavior of the queue except that
        --  every item put in "really ought to" come out sometime before
        --  'dequeue' returns a 'Nothing'.
        enqueue :: q -> a -> m ()
        enqueueBatch :: q -> [a] -> m ()
        enqueueBatch q = mapM_ (enqueue q)
        
class Monad m => Dequeue q m a | q -> a where
        -- |Pull an item out of a queue.  Should not block.  No ordering
        --  constraints are implied other than that any item that went into
        --  the queue "really ought to" come out before 'dequeue' returns
        --  'Nothing'.
        dequeue :: q -> m (Maybe a)
        dequeueBatch :: q -> m [a]
        dequeueBatch q = do
                x <- dequeue q
                return $ case x of { Just x -> [x]; Nothing -> [] }

class Monad m => DequeueWhere q m a | q -> a where
        -- |Pull an item matching the given predicate out of a queue.
        dequeueWhere :: q -> (a -> Bool) -> m (Maybe a)

class Monad m => PeekQueue q m a | q -> a where
        -- |return the whole contents of the queue (if possible) without 
        --  altering the queue's contents.  Obviously in cases where this
        --  can't be done lazily this can be a very expensive operation.
        peekQueue :: q -> m [a]
        
        -- |peek a specified number of items off the queue.  The default
        --  implementation is hideously wasteful in cases where peekQueue is
        --  not able to get the contents lazily.
        peekQueueTaking :: Int -> q -> m [a]
        peekQueueTaking n q = do
            xs <- peekQueue q
            return (take n xs)

class Monad m => QueueSize q m where
        -- |return the number of elements in the queue
        queueSize :: q -> m Int
