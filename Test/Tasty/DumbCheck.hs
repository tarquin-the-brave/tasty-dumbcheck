{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Tasty.DumbCheck
  ( testProperty
  , module Test.DumbCheck
  ) where

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

testProperty :: Testable a => TestName -> a -> TestTree
testProperty name t = singleTest name (DumbTest $ test t)

instance IsOption DumbCheckTests where
  defaultValue = 1000
  parseValue = fmap DumbCheckTests . safeRead
  optionName = return "dumbcheck-tests"
  optionHelp = return "Total number of tests to run"

newtype DumbTest = DumbTest { unDumbTest :: Int -> Either String Int }

instance IsTest DumbTest where

    testOptions = return [Option (Proxy :: Proxy DumbCheckTests)]

    run opts t _ =
        pure . either (testFailed . ("Failed for : " <>) . show)
                      (testPassed . ("Completed tests: " <>) . show)
             . unDumbTest t $ n
      where
        DumbCheckTests n = lookupOption opts
