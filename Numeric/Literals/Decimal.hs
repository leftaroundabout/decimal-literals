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

instance Show FractionalLit where
  showsPrec p (ExactRatio r)
   | denominator r > 1  = showParen (p>6)
                 $ showsPrec 7 (numerator r) . ('/':) . showsPrec 7 (denominator r)
   | otherwise          = shows $ numerator r
  showsPrec p (DecimalFraction m e)
   | m < 0             = showParen (p>5) $ ('-':) . shows (DecimalFraction (-m) e)
   | e > 6 || e < -2   = case show m of
               lsh | e<0, length lsh + 1 > -e
                           -> case splitAt (-e) $ reverse lsh of
                             (acs, []) -> ("0."++) . (replicate (-e-length acs) '0'++)
                                           . (reverse acs++)
                             (acs, pcs) -> (reverse pcs++) . ('.':) . (reverse acs++)
               (hd:[]) -> (hd:) . ("e"++) . shows e
               (hd:lds) -> (hd:) . ('.':) . (lds++) . ("e"++) . shows (e + length lds)
   | e < 0             = case splitAt (-e) . reverse $ show m of
               (acs, [])  -> ("0."++) . (replicate (-e-length acs) '0'++) . (reverse acs++)
               (acs, pcs) -> (reverse pcs++) . ('.':) . (reverse acs++)
   | otherwise         = shows m . (replicate e '0'++)

infixl 7 `unbiasedDiv`
unbiasedDiv :: Integral a => a -> a -> a
unbiasedDiv x y = (x - y`quot`2)`div`y

instance Num FractionalLit where
  fromInteger = ExactRatio . fromInteger
  ExactRatio r₀ + ExactRatio r₁ = ExactRatio $ r₀ + r₁
  DecimalFraction m e + ExactRatio r
      = DecimalFraction (m + round (10/2^^e)) e
  DecimalFraction m₀ e₀ + DecimalFraction m₁ e₁
   | e₀ <= e₁  = DecimalFraction (m₀`unbiasedDiv`10^(e₁ - e₀) + m₁) e₁
  n + m = m + n
  ExactRatio r₀ * ExactRatio r₁ = ExactRatio $ r₀ * r₁
  DecimalFraction m e * ExactRatio r
     = DecimalFraction ((m * numerator r)`unbiasedDiv`denominator r) e
  DecimalFraction m₀ e₀ * DecimalFraction m₁ e₁
        = DecimalFraction (m₀*m₁) (e₀+e₁)
  n * m = m * n
  negate (ExactRatio r) = ExactRatio $ -r
  negate (DecimalFraction m e) = DecimalFraction (-m) e

instance Fractional FractionalLit where
  fromRational r
    | r < 0               = negate $ fromRational (-r)
    | denominator r == 1  = goI 0 0 $ numerator r
    | otherwise           = goF 0 0 $ denominator r
   where goI n2 n5 u
          | (u', 0) <- u`divMod`2  = goI (n2+1) n5 u'
          | (u', 0) <- u`divMod`5  = goI n2 (n5+1) u'
          | n2 > 3 && n5 > n2      = DecimalFraction (u*5^(n5-n2)) n2
          | n5 > 3 && n2 >= n5     = DecimalFraction (u*2^(n2-n5)) n5
          | otherwise              = ExactRatio r
         goF 0 0 1     = ExactRatio r
         goF n2 n5 1
          | n2>n5      = DecimalFraction (numerator r*5^(n2-n5)) (-n2)
          | otherwise  = DecimalFraction (numerator r*2^(n5-n2)) (-n5)
         goF n2 n5 d
          | (d', 0) <- d`divMod`2  = goF (n2+1) n5 d'
          | (d', 0) <- d`divMod`5  = goF n2 (n5+1) d'
          | otherwise              = ExactRatio r
