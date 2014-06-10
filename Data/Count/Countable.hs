module Data.Count.Countable where

import Data.Count.Counter

import Data.Int

class Countable a where
  counter :: Counter a

toPos :: Countable a => a -> Integer
fromPos :: Countable a => Integer -> a
count :: Countable a => a -> Maybe Integer

toPos = cToPos counter
fromPos = cFromPos counter
count a = cCount c
  where
    c = counter -- monomorphise
    constr = a `asTypeOf` cFromPos c 0

allValues :: Countable a => [a]
allValues = allValuesFor counter

instance Countable Integer where
  counter = integerCounter

instance Countable Bool where
  counter = boundedEnumCounter

instance Countable Char where
  counter = boundedEnumCounter

-- not portable
instance Countable Int where
  counter = boundedEnumCounter

instance Countable Int8 where
  counter = boundedEnumCounter

instance Countable Int16 where
  counter = boundedEnumCounter

instance Countable Int32 where
  counter = boundedEnumCounter

instance Countable Int64 where
  counter = boundedEnumCounter

instance Countable () where
  counter = unitCounter

instance (Countable a, Countable b) => Countable (Either a b) where
  counter = sumCounter counter counter

instance (Countable a, Countable b) => Countable (a, b) where
  counter = prodCounter counter counter

instance (Countable a, Countable b, Countable c) => Countable (a, b, c) where
  counter = isoCounter (prodCounter counter (prodCounter counter counter)) f g
    where
      f (a, b, c) = (a, (b, c))
      g (a, (b, c)) = (a, b, c)

instance (Countable a, Countable b, Countable c, Countable d) => Countable (a, b, c, d) where
  counter = isoCounter (prodCounter counter (prodCounter counter (prodCounter counter counter))) f g
    where
      f (a, b, c, d) = (a, (b, (c, d)))
      g (a, (b, (c, d))) = (a, b, c, d)

class Countable1 f where
  counter1 :: Counter a -> Counter (f a)

instance Countable a => Countable [a] where
  counter = listCounter counter

instance Countable a => Countable (Maybe a) where
  counter = maybeCounter counter
