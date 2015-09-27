{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ <= 710
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Test.Tasty.DumbCheck
  ( testSeriesProperty
  , testSerialProperty
  , testBoolProperty
  , module Test.DumbCheck
  ) where

import Control.Arrow (left)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)

import Test.Tasty.Options
  ( IsOption (defaultValue, parseValue, optionName, optionHelp)
  , OptionDescription (Option)
  , lookupOption
  , safeRead
  )
import Test.Tasty.Providers
  ( IsTest (testOptions, run)
  , TestName
  , TestTree
  , singleTest
  , testFailed
  , testPassed
  )

import Test.DumbCheck

newtype DumbCheckTests = DumbCheckTests Int
  deriving (Num, Ord, Eq, Real, Enum, Integral, Typeable)

testSeriesProperty
  :: Show a => TestName -> Property a -> Series a -> TestTree
testSeriesProperty name p =
    singleTest name . DumbTest . (left show .) . checkSeries p

testSerialProperty
  :: (Serial a, Show a) => TestName -> Property a -> TestTree
testSerialProperty name p = testSeriesProperty name p series

testBoolProperty :: TestName -> Series Bool -> TestTree
testBoolProperty name ss =
    singleTest name . DumbTest
                    $ \n -> maybe (Right n) (Left . show) $ checkBools ss n

instance IsOption DumbCheckTests where
  defaultValue = 1000
  parseValue = fmap DumbCheckTests . safeRead
  optionName = return "dumbcheck-tests"
  optionHelp = return "Total number of tests to run"

newtype DumbTest = DumbTest { unDumbTest :: Int -> Either String Int }
                   deriving (Typeable)

instance IsTest DumbTest where

    testOptions = return [Option (Proxy :: Proxy DumbCheckTests)]

    run opts t _ =
        pure . either (testFailed . ("Failed for : " <>) . show)
                      (testPassed . ("Completed tests: " <>) . show)
             . unDumbTest t $ n
      where
        DumbCheckTests n = lookupOption opts
