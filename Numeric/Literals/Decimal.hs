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


-- | A type whose 'Fractional' instance gives a somewhat reliable indication whether
--   a value was actually defined as an integer or a ratio, or as a decimal-fraction
--   literal. This is useful to know for a type that supports both exact fraction
--   values and more floating-point-like / physical values; it allows avoiding
--   things like @0.524@ showing up as @589971551185535/1125899906842624@, or conversely
--   @7/23@ as @0.30434782608695654@. Both of these scenarios are quite awkward.
data FractionalLit = ExactRatio Rational
                   | DecimalFraction String

