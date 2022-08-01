{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where 

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta 

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

shouldAlsoFail = "234/23"

parseFraction :: Parser Rational 
parseFraction = do  
  numerator <- decimal 
  -- the result of the monadic action is saved to 'numerator'
  char '/'
  -- the result of the monadic action of 'char' is not saved.
  denominator <- decimal 
  return (numerator % denominator) 

-- This parses a non-negative integer followed by a forward slash followed by another non-negative integer and returns the two parsed non-negative integers as a ratio.

main :: IO ()
main = do 
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty shouldAlsoFail
  print $ parseString parseFraction mempty badFraction
  -- Anything after this point won't run due to the runtime exception produced by the above line, namely, *** Exception: Ratio has zero denominator, which ends our program.
  print $ "this side-effect will not appear"


virtuousParseFraction :: Parser Rational 
virtuousParseFraction = do
  numerator <- decimal 
  char '/'   
  denominator <- decimal 
  case denominator of 
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do 
  print $ parseString virtuousParseFraction mempty badFraction
  print $ parseString virtuousParseFraction mempty shouldWork 
  print $ parseString virtuousParseFraction mempty shouldAlsoWork
  print $ parseString virtuousParseFraction mempty alsoBad
  print $ parseString virtuousParseFraction mempty shouldAlsoFail
 