{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ <= 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Test.DumbCheck where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Applicative (ZipList(ZipList,getZipList))
import Control.Monad (replicateM)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Monoid (Sum(..))

type Series a = [a]

type Property a = a -> Bool

class Serial a where
    series :: Series a

instance Serial () where
    series = pure ()

instance Serial Bool where
    series = [True,False]

instance Serial Int where
    -- No `Monad` for `ZipList`
    series = (0:) . concat . getZipList
           $ (\x y -> [x,y]) <$> ZipList [1 .. maxBound]
                             <*> ZipList [-1, -2 .. minBound]

newtype Positive = Positive { unPositive :: Int }

instance Serial Positive where
    series = Positive <$> [1..maxBound]

newtype Negative = Negative { unNegative :: Int }

instance Serial Negative where
    series = Negative <$> [-1,-2 .. minBound]

instance (Enum a, Num a) => Serial (Sum a) where
    series = Sum <$> [0..]

instance Serial Char where
    series = ['\NUL'..]

instance Serial a => Serial (Maybe a) where
    series = Nothing : (Just <$> series)

instance Serial a => Serial [a] where
    series = concatMap (`replicateM` series) [0..]

instance (Serial a, Serial b) => Serial (a,b) where
    series = (,) <$> series <*> series

instance (Serial a, Serial b, Serial c) => Serial (a,b,c) where
    series = (,,) <$> series <*> series <*> series

instance (Serial a, Serial b, Serial c, Serial d) => Serial (a,b,c,d) where
    series = (,,,) <$> series <*> series <*> series <*> series

instance Serial b => Serial (a -> b) where
    series = const <$> series

-- * Testable

class Testable a where
    test :: a -> Int -> Either String Int

instance Testable (Series Bool) where
    test ss n = maybe (Right n) (Left . show) $ (elemIndex False . take n) ss

instance (Show a, Serial a) => Testable (Property a) where
    -- TODO: return number of tests taken
    test p = test (p, series :: Series a)

instance (Serial a, Show a) => Testable (Property a, Series a) where
    -- TODO: return number of tests taken
    test (p,ss) n = maybe (Right n) (Left . show) $ find (not . p) (take n ss)

-- TODO: return number of tests taken
testBool :: Series Bool -> Int -> Maybe Int
testBool ss n = (elemIndex False . take n) ss

-- TODO: return number of tests taken
testSeries :: Property a -> Series a -> Int -> Either a Int
testSeries p ss n = maybe (Right n) Left $ find (not . p) (take n ss)

testSerial :: Serial a => Property a -> Int -> Either a Int
testSerial = flip testSeries series
