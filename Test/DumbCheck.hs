module Test.DumbCheck where

import Control.Applicative (liftA3, ZipList(ZipList,getZipList))
import Control.Monad (replicateM)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Monoid (Sum(..), (<>))

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
    series = (0:) . mconcat . getZipList
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

-- TODO: return number of tests taken
testBool :: Series Bool -> Int -> Maybe Int
testBool ss n = (elemIndex False . take n) ss

-- TODO: return number of tests taken
test :: Serial a => Property a -> Series a -> Int -> Either a Int
test p ss n = maybe (Right n) Left $ find (not . p) (take n ss)

-- * Examples

leftIdentity :: (Eq a, Monoid a) => Property a
leftIdentity x = mempty <> x == x

associativity :: (Eq a, Monoid a) => Property (a,a,a)
associativity (x,y,z) = x <> y <> z == x <> y <> z

associativity' :: (Eq a, Monoid a)
               => Series a
               -> Series a
               -> Series a
               -> Series Bool
associativity' = liftA3 $ \x y z -> x <> y <> z == x <> y <> z


composition :: (Eq (f b), Functor f) => Property (f c, a -> b, c -> a)
composition (x,f,g) = fmap (f . g) x == (fmap f . fmap g) x

composition' :: (Eq (f b), Functor f)
             => Series (f c)
             -> Series (a -> b)
             -> Series (c -> a)
             -> Series Bool
composition' = liftA3 $ \x f g -> fmap (f . g) x == (fmap f . fmap g) x

testAssociativity = test associativity (series :: Series (Sum Int, Sum Int, Sum Int)) 1000

testAssociativity' = testBool (associativity' (series :: Series (Sum Int)) series series) 100

testComposition = test composition (series :: Series ((Maybe Int)
                                            , Int -> Int
                                            , Int -> Int)) 1000

testComposition' = testBool (composition' (series :: Series (Maybe Int))
                                          (series :: Series (Int -> Int))
                                          (series :: Series (Int -> Int))) 100
