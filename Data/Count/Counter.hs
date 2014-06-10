module Data.Count.Counter where

import Control.Applicative ((<$>), (<*>))
import Data.Tuple (swap)

-- | A @'Counter' a@ maps bijectively between a subset of values of type @a@ and some possibly empty or infinite prefix of @[0..]@.
--
-- 'cCount' is @'Just' n@ when the counter is finite and manages @n@ values, or @'Nothing'@ when infinite.
--
-- 'cToPos' converts a managed value to its natural number (starting from 0).
--
-- 'cFromPos' converts a natural number to its managed value.
--
-- @'cToPos' c . 'cFromPos' c@ must be the identity function. This invariant is maintained using the combinators below.
data Counter a = UnsafeMkCounter {
  cCount :: Maybe Integer,
  cToPos :: a -> Integer,
  cFromPos :: Integer -> a
}

-- | A counter for the single unit value.
unitCounter :: Counter ()
unitCounter =
  UnsafeMkCounter {
    cCount = Just 1,
    cToPos = \() -> 0,
    cFromPos = \0 -> ()
  }

-- | A counter for an empty set of values, for any type. 
voidCounter :: Counter a
voidCounter =
  UnsafeMkCounter {
    cCount = Just 0,
    cToPos = const undefined,
    cFromPos = const undefined
  }

-- | Counts through the natural numbers: @[0..]@ maps simply to @[0..]@.
natCounter :: Counter Integer
natCounter =
  UnsafeMkCounter {
    cCount = Nothing,
    cToPos = id,
    cFromPos = id
  }

-- | @'dropCounter' n c@ drops the first @n@ elements from the given counter. @'cToPos' ('dropCounter' n c) 0@ is equivalent to @'cToPos' c 0@.
dropCounter :: Integer -> Counter a -> Counter a
dropCounter skip aC =
  UnsafeMkCounter {
    cCount = max 0 . subtract skip <$> cCount aC,
    cToPos = subtract skip . cToPos aC,
    cFromPos = cFromPos aC . (+skip)
  }

-- | Given two counters, @a@ and @b@, creates a counter for all 'Left'-tagged @a@ values and 'Right'-tagged @b@ values.
sumCounter :: Counter a -> Counter b -> Counter (Either a b)
sumCounter aC bC =
  UnsafeMkCounter {
    cCount = (+) <$> cCount aC <*> cCount bC,

    cToPos = case (cCount aC, cCount bC) of
      (Nothing, Nothing) -> \ab -> case ab of
        Left a -> 2*cToPos aC a
        Right b -> 2*cToPos bC b + 1

      (Just acount, _) -> \ab -> case ab of
        Left a -> cToPos aC a
        Right b -> acount + cToPos bC b

      (Nothing, Just bcount) -> cToPos (sumCounter bC aC) . invert,

    cFromPos = case (cCount aC, cCount bC) of
      (Nothing, Nothing) -> \n -> case n `divMod` 2 of
        (n', 0) -> Left $ cFromPos aC $ n'
        (n', 1) -> Right $ cFromPos bC $ n'

      (Just acount, _) -> \n -> if n < acount
        then Left $ cFromPos aC $ n
        else Right $ cFromPos bC $ n - acount

      (Nothing, Just _) -> invert . cFromPos (sumCounter bC aC)
  }
  where
    invert m = case m of
      Left a -> Right a
      Right a -> Left a
      
-- | Creates a counter for the Cartesian product of values in two given counters.
prodCounter :: Counter a -> Counter b -> Counter (a, b)
prodCounter aC bC =
  UnsafeMkCounter {
    cCount = if Just 0 `elem` [cCount aC, cCount bC]
      then Just 0 -- 0*infinity = 0
      else (*) <$> cCount aC <*> cCount bC,

    cToPos = case (cCount aC, cCount bC) of
      (Nothing, Nothing) -> posf $ \(an, bn) -> tri (an + bn) + an

      (_, Just bcount) -> posf $ \(an, bn) -> an*bcount + bn

      (Just _, Nothing) -> cToPos (prodCounter bC aC) . swap,

    cFromPos = case (cCount aC, cCount bC) of
      (Nothing, Nothing) -> \n -> let (tpos, rpos) = rtri n in
        (cFromPos aC rpos, cFromPos bC (tpos - rpos))

      (_, Just bcount) -> \n -> let (an, bn) = n `divMod` bcount in
        (cFromPos aC an, cFromPos bC bn)

      (Just _, Nothing) -> swap . cFromPos (prodCounter bC aC)
  }
  where
    posf f (a, b) = f (cToPos aC a, cToPos bC b)

    tri :: Integer -> Integer
    tri n = n*(n + 1) `div` 2

    rtri :: Integer -> (Integer, Integer)
    rtri n = 
      (r, n - tri r)
      where
        -- from https://oeis.org/A003056 -- Antti Karttunen
        r = (squareRoot (1 + 8*n) - 1) `div` 2
        sq n = n*n
        -- from http://www.haskell.org/haskellwiki/Generic_number_type
        squareRoot 0 = 0
        squareRoot 1 = 1
        squareRoot n =
           let twopows = iterate sq 2
               (lowerRoot, lowerN) =
                  last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
               newtonStep x = div (x + div n x) 2
               iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
               isRoot r  =  sq r <= n && n < sq (r+1)
           in  head $ dropWhile (not . isRoot) iters

-- | A counter for any 'Bounded' 'Enum'. @['minBound' :: a ..]@ maps to @[0..]@.
boundedEnumCounter :: (Bounded a, Enum a) => Counter a
boundedEnumCounter = counter
  where
    [min, max] = map (toInteger . fromEnum) [minBound, maxBound `asTypeOf` cFromPos counter 0]
    counter = UnsafeMkCounter {
      cCount = Just $ max - min + 1,
      cToPos = \v -> (toInteger . fromEnum) v - min,
      cFromPos = \n -> toEnum . fromInteger $ min + n
    }

isoCounter :: Counter a -> (b -> a) -> (a -> b) -> Counter b
isoCounter aC b2a a2b =
  UnsafeMkCounter {
    cCount = cCount aC,
    cToPos = cToPos aC . b2a,
    cFromPos = a2b . cFromPos aC
  }

maybeCounter :: Counter a -> Counter (Maybe a)
maybeCounter aC = isoCounter (sumCounter aC unitCounter) f g
  where
    f m = case m of
      Just a -> Left a
      Nothing -> Right ()
    g e = case e of
      Left a -> Just a
      Right () -> Nothing

-- | Counter for all lists of all values in given counter.
--
-- The count is 1 (the only value being the empty list) if the given counter is empty, infinite otherwise.
listCounter :: Counter a -> Counter [a]
listCounter aC =
  counter
  where
    -- Counter (Either (@aC, [a]) ())
    inner = sumCounter (prodCounter aC counter) unitCounter
    count = succ <$> cCount (prodCounter aC integerCounter)
    -- override recursive count
    counter = (isoCounter inner fromLs toLs){ cCount = count }
    fromLs l = case l of
      (a:as) -> Left (a, as)
      [] -> Right ()
    toLs e = case e of
      Left (a, as) -> (a:as)
      Right () -> []

-- | Maps [0,1,-1,2,-2,..] to [0..]
integerCounter :: Counter Integer
integerCounter =
  UnsafeMkCounter {
    cCount = Nothing,
    cToPos = \i -> if i > 0
      then i*2 - 1
      else abs i*2,
    cFromPos = \n -> case (n + 1) `divMod` 2 of
      (n', 0) -> n'
      (n', 1) -> negate n'
  }

-- | All values in the given counter, from the @0@ correspondent upwards.
allValuesFor :: Counter a -> [a]
allValuesFor aC =
  map (cFromPos aC) range
  where
    range = case cCount aC of
      Just n -> [0..n - 1]
      Nothing -> [0..]
