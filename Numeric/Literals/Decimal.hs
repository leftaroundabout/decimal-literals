-- |
-- Module      : Numeric.Literals.Decimal
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Numeric.Literals.Decimal
          ( FractionalLit
          , pattern (:%)
          , pattern Scientific
          ) where

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

asFraction :: FractionalLit -> Maybe (Integer, Integer)
asFraction (ExactRatio r) = Just (numerator r, denominator r)
asFraction (DecimalFraction _ _) = Nothing

pattern (:%) :: Integer -> Integer -> FractionalLit
pattern n:%d <- (asFraction -> Just (n,d))
 where n:%d = ExactRatio $ fromInteger n % fromInteger d

asScientific :: FractionalLit -> Maybe ((Int, [B₁₀Digit]), Int)
asScientific (ExactRatio _) = Nothing
asScientific n = case break (=='e') $ show n of
      (m, 'e':e) -> Just (splitMantissa m, read e)
      (m, [])    -> Just (splitMantissa m, 0)
 where splitMantissa m = case break (=='.') m of
         (pm,'.':am) -> (read pm, toEnum . fromEnum<$>am)
         (pm,[])     -> (read pm, [])

pattern Scientific :: Int        -- ^ Integral part of the mantissa
                   -> [B₁₀Digit] -- ^ Digits after the point of the mantissa
                   -> Int        -- ^ Base-10 exponent of the number in scientific form
                   -> FractionalLit
pattern Scientific pc ac ex <- (asScientific -> Just ((pc,ac),ex))
 where Scientific pc ac ex = DecimalFraction (nqr (fromIntegral pc) ac) $ ex - length ac
        where nqr n [] = n
              nqr n (d:ds) = nqr (n*10 + fromIntegral (fromEnum d)) ds
              

data B₁₀Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Enum, Eq)
instance Show B₁₀Digit where show = show . fromEnum
instance Num B₁₀Digit where
  fromInteger = toEnum . (`mod`10) . fromInteger
  d + e = toEnum $ (fromEnum d + fromEnum e)`mod`10
  d * e = toEnum $ (fromEnum d * fromEnum e)`mod`10
  d - e = toEnum $ (fromEnum d - fromEnum e)`mod`10
  abs = id
  signum D0 = 0
  signum _ = 1

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
unbiasedDiv x y = (x + y`quot`2)`div`y

instance Num FractionalLit where
  fromInteger = ExactRatio . fromInteger
  ExactRatio r₀ + ExactRatio r₁ = ExactRatio $ r₀ + r₁
  DecimalFraction m e + ExactRatio r
      = DecimalFraction (m + round (r * 10^^(-e))) e
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
  abs (ExactRatio r) = ExactRatio $ abs r
  abs (DecimalFraction m e) = DecimalFraction (abs m) e
  signum (ExactRatio r) = ExactRatio $ signum r
  signum (DecimalFraction m e) = ExactRatio . fromIntegral $ signum m

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
  ExactRatio r₀ / ExactRatio r₁ = ExactRatio $ r₀ / r₁
  DecimalFraction m e / ExactRatio r
     = DecimalFraction ((m * denominator r)`unbiasedDiv`numerator r) e
  ExactRatio r / DecimalFraction m e
     = DecimalFraction (round $ r * 10^dp / fromIntegral m) (-dp-e)
   where dp = ceiling . logBase 10 $ fromIntegral m
  DecimalFraction m₀ e₀ / DecimalFraction m₁ e₁
        = DecimalFraction ((m₀*10^dp)`unbiasedDiv`m₁) (e₀-e₁-dp)
   where dp = ceiling . logBase 10 $ fromIntegral m₁
