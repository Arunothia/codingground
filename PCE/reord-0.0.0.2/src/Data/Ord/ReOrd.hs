{-
 -      ``Util/ReOrd.hs''
 -}

module Data.Ord.ReOrd where

-- |A handy constructor which just reverses the sense of an existing 'Ord'
--  instance.
newtype Ord a => ReverseOrd a = ReverseOrd a
        deriving (Eq, Show)

instance (Ord a) => Ord (ReverseOrd a) where
        compare (ReverseOrd a) (ReverseOrd b) = compare b a

-- |A type which provides an ad-hoc 'Ord' instance for the type it wraps.
-- It is the user's responsibility to make sure that it obeys all
-- relevant laws, also taking into account the fact that when 2 items
-- are compared, only one of their 'cmp' functions is invoked (the left one)
data ReOrd a = 
        ReOrd { cmp   :: a -> a -> Ordering
              , item  :: a
              }

instance Eq (ReOrd a) where
        a == b  = case cmp a (item a) (item b) of
                EQ -> True
                __ -> False
        a /= b  = case cmp a (item a) (item b) of
                EQ -> False
                __ -> True

instance Ord (ReOrd a) where
        compare a b = cmp a (item a) (item b)
