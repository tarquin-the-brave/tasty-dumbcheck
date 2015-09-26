module Main where

import Control.Applicative (liftA3)
import Data.Monoid -- ((<>))

import Test.Tasty

import Test.Tasty.DumbCheck

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testProperty "Left Identity" (leftIdentity :: Property (Sum Int))
    , testProperty "Associativity" (associativity :: Property (Sum Int, Sum Int, Sum Int))
    , testProperty "Composition" $ composition' (series :: Series (Maybe Int))
                                                (series :: Series (Int -> Int))
                                                (series :: Series (Int -> Int))
    ]

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

-- testAssociativity = test associativity (series :: Series (Sum Int, Sum Int, Sum Int)) 1000

-- testAssociativity' = testBool (associativity' (series :: Series (Sum Int)) series series) 100

-- testComposition = test composition (series :: Series ((Maybe Int)
--                                             , Int -> Int
--                                             , Int -> Int)) 1000

-- testComposition' = testBool (composition' (series :: Series (Maybe Int))
--                                           (series :: Series (Int -> Int))
--                                           (series :: Series (Int -> Int))) 100
