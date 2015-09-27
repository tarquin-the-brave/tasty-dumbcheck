{-# LANGUAGE CPP #-}
module Main where

import Control.Applicative (liftA3)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(mempty))
#endif
import Data.Monoid (Sum, (<>))

import Test.Tasty

import Test.Tasty.DumbCheck

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testSerialProperty "Left Identity" (leftIdentity :: Property (Sum Int))
    , testSerialProperty "Associativity" (associativity :: Property (Sum Int, Sum Int, Sum Int))
    , testBoolProperty "Composition"
      $ composition' (series :: Series (Maybe Int))
                     (series :: Series (Int -> Int))
                     (series :: Series (Int -> Int))
    ]

leftIdentity :: (Eq a, Monoid a) => Property a
leftIdentity x = mempty <> x == x

associativity :: (Eq a, Monoid a) => Property (a,a,a)
associativity (x,y,z) = x <> y <> z == x <> y <> z

associativity'
  :: (Eq a, Monoid a) => Series a -> Series a -> Series a -> Series Bool
associativity' = liftA3 $ \x y z -> x <> y <> z == x <> y <> z


composition :: (Eq (f b), Functor f) => Property (f c, a -> b, c -> a)
composition (x,f,g) = fmap (f . g) x == (fmap f . fmap g) x

composition'
  :: (Eq (f b), Functor f)
  => Series (f c) -> Series (a -> b) -> Series (c -> a) -> Series Bool
composition' = liftA3 $ \x f g -> fmap (f . g) x == (fmap f . fmap g) x
