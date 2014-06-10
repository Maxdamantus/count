module Data.Count.Counter where

import Control.Applicative ((<$>), (<*>))
import Data.Tuple (swap)

data Counter a = UnsafeMkCounter {
  cCount :: Maybe Integer,
  cToPos :: a -> Integer,
  cFromPos :: Integer -> a
}

unitCounter :: Counter ()
unitCounter =
  UnsafeMkCounter {
    cCount = Just 1,
    cToPos = \() -> 0,
    cFromPos = \0 -> ()
  }

voidCounter :: Counter a
voidCounter =
  UnsafeMkCounter {
    cCount = Just 0,
    cToPos = const undefined,
    cFromPos = const undefined
  }

natCounter :: Counter Integer
natCounter =
  UnsafeMkCounter {
    cCount = Nothing,
    cToPos = id,
    cFromPos = id
  }

dropCounter :: Integer -> Counter a -> Counter a
dropCounter skip aC =
  UnsafeMkCounter {
    cCount = max 0 . subtract skip <$> cCount aC,
    cToPos = subtract skip . cToPos aC,
    cFromPos = cFromPos aC . (+skip)
  }

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
        (n', 0) -> Left $ cFromPos aC $ n
        (n', 1) -> Right $ cFromPos bC $ n

      (Just acount, _) -> \n -> if n < acount
        then Left $ cFromPos aC $ n
        else Right $ cFromPos bC $ n - acount

      (Nothing, Just _) -> invert . cFromPos (sumCounter bC aC)
  }
  where
    invert m = case m of
      Left a -> Right a
      Right a -> Left a
      
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


boundedEnumCounter :: (Bounded a, Enum a) => Counter a
boundedEnumCounter = counter
  where
    [min, max] = map (toInteger . fromEnum) [minBound, maxBound `asTypeOf` cFromPos counter 0]
    counter = UnsafeMkCounter {
      cCount = Just $ max - min + 1,
      cToPos = \v -> (toInteger . fromEnum) v - min,
      cFromPos = \n -> toEnum . fromInteger $ min + n
    }

unsafeIsoCounter :: Counter a -> (Maybe Integer) -> (b -> a) -> (a -> b) -> Counter b
unsafeIsoCounter aC count b2a a2b =
  UnsafeMkCounter {
    cCount = count,
    cToPos = cToPos aC . b2a,
    cFromPos = a2b . cFromPos aC
  }

isoCounter :: Counter a -> (b -> a) -> (a -> b) -> Counter b
isoCounter aC = unsafeIsoCounter aC (cCount aC)

maybeCounter :: Counter a -> Counter (Maybe a)
maybeCounter aC = isoCounter (sumCounter aC unitCounter) f g
  where
    f m = case m of
      Just a -> Left a
      Nothing -> Right ()
    g e = case e of
      Left a -> Just a
      Right () -> Nothing

listCounter :: Counter a -> Counter [a]
listCounter aC =
  counter
  where
    -- Counter (Either (@aC, [a]) ())
    inner = sumCounter (prodCounter aC counter) unitCounter
    count = succ <$> cCount (prodCounter aC integerCounter)
    counter = unsafeIsoCounter inner count fromLs toLs
    fromLs l = case l of
      (a:as) -> Left (a, as)
      [] -> Right ()
    toLs e = case e of
      Left (a, as) -> (a:as)
      Right () -> []

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

allValuesFor :: Counter a -> [a]
allValuesFor aC =
  map (cFromPos aC) range
  where
    range = case cCount aC of
      Just n -> [0..n - 1]
      Nothing -> [0..]
