-- |
-- Module      : Numeric.Literals.Decimal
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Numeric.Literals.Decimal where

import Data.Ratio


-- | A type whose 'Fractional' instance gives a somewhat reliable indication whether
--   a value was actually defined as an integer or a ratio, or as a decimal-fraction
--   literal. This is useful to know for a type that supports both exact fraction
--   values and more floating-point-like / physical values; it allows avoiding
--   things like @0.524@ showing up as @589971551185535/1125899906842624@, or conversely
--   @7/23@ as @0.30434782608695654@. Both of these scenarios are quite awkward.
data FractionalLit = ExactRatio Rational
                   | DecimalFraction {
                       decimalMantissa :: Integer
                     , decimalExponent :: Int }

instance Eq FractionalLit where
  ExactRatio r₀ == ExactRatio r₁  =  r₀==r₁
  DecimalFraction m₀ e₀ == DecimalFraction m₁ e₁  =  m₀==m₁ && e₀==e₁
  ExactRatio r == DecimalFraction m e
   | e>0        = ExactRatio r == DecimalFraction (m*10^e) 0
   | (ers,0) <- (10^(-e)) `divMod` denominator r
                =  m == numerator r*ers
   | otherwise  =  False
  n == m  =  m == n

