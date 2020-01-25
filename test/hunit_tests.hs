{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.HUnit

import Prelude hiding (length)
import Control.Monad (when)
import Data.Foldable (length)
import Data.Validation (Validation (Success, Failure), ensure,
                        orElse, validate, validation, validationNel)
import System.Exit (exitFailure)

seven :: Int
seven = 7

three :: Int
three = 3

four :: Int
four = 4

testYY :: Test
testYY =
  let subject  = Success (+1) <*> Success seven :: Validation String Int
      expected = Success 8
  in  TestCase (assertEqual "Success <*> Success" subject expected)

testNY :: Test
testNY =
  let subject  = Failure ["f1"] <*> Success seven :: Validation [String] Int
      expected = Failure ["f1"]
  in  TestCase (assertEqual "Failure <*> Success" subject expected)

testYN :: Test
testYN =
  let subject  = Success (+1) <*> Failure ["f2"] :: Validation [String] Int
      expected = Failure ["f2"]
  in  TestCase (assertEqual "Success <*> Failure" subject expected)

testNN :: Test
testNN =
  let subject  = Failure ["f1"] <*> Failure ["f2"] :: Validation [String] Int
      expected = Failure ["f1","f2"]
  in  TestCase (assertEqual "Failure <*> Failure" subject expected)

testValidationNel :: Test
testValidationNel =
  let subject  = validation length (const 0) $ validationNel (Left ())
  in  TestCase (assertEqual "validationNel makes lists of length 1" subject 1)

testEnsureLeftNothing, testEnsureLeftJust, testEnsureRightNothing,
 testEnsureRightJust, testEnsureRightJust', testOrElseRight, testOrElseLeft
  :: Test

testEnsureLeftNothing =
  let subject :: Validation Int Int
      subject = ensure three (const Nothing) (Failure seven)
  in  TestCase (assertEqual "ensure Left False" subject (Failure seven))

testEnsureLeftJust =
  let subject :: Validation Int Int
      subject = ensure three (Just . id) (Failure seven)
  in  TestCase (assertEqual "ensure Left True" subject (Failure seven))

testEnsureRightNothing =
  let subject :: Validation Int Int
      subject = ensure three (const Nothing) (Success seven)
  in  TestCase (assertEqual "ensure Right False" subject (Failure three))

testEnsureRightJust =
  let subject :: Validation Int Int
      subject = ensure three (Just . id) (Success seven)
  in  TestCase (assertEqual "ensure Right True" subject (Success seven))

testEnsureRightJust' =
  let subject :: Validation Int Int
      subject = ensure three (const $ Just four) (Success seven)
  in  TestCase (assertEqual "ensure Right True" subject (Success four))

testOrElseRight =
  let v :: Validation Int Int
      v = Success seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseRight" subject seven)

testOrElseLeft =
  let v :: Validation Int Int
      v = Failure seven
      subject = v `orElse` three
  in  TestCase (assertEqual "orElseLeft" subject three)

testValidateJust :: Test
testValidateJust =
  let subject = validate three (Just . id) seven
      expected = Success seven
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateJust' :: Test
testValidateJust' =
  let subject = validate three (const $ Just four) seven
      expected = Success four
  in  TestCase (assertEqual "testValidateTrue" subject expected)

testValidateNothing :: Test
testValidateNothing =
  let subject = validate three (const option) seven
      expected = Failure three
      option = Nothing :: Maybe Int
  in  TestCase (assertEqual "testValidateFalse" subject expected)

tests :: Test
tests = TestList $
  [ testYY
  , testYN
  , testNY
  , testNN
  , testValidationNel
  , testValidateNothing
  , testValidateJust
  , testValidateJust'
  , testEnsureLeftNothing
  , testEnsureLeftJust
  , testEnsureRightNothing
  , testEnsureRightJust
  , testEnsureRightJust' 
  , testOrElseLeft
  , testOrElseRight
  ]

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) exitFailure
