module Data.GosperBase where
import Data.Array.Unboxed
import Data.Word
import Data.GosperBase.Internals
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

{- This computes complex numbers in base 2.5-√(-3/4), called the Gosper base
   because it is the scale factor from one Gosper island to the next bigger one.
   The digits are cyclotomic:
    2 3
   6 0 1
    4 5
   For layout of all numbers up to 3 digits, see doc/GosperBase.ps .
-}

{-
  A mantissa is a sequence of limbs. There are two kinds: right-justified,
  for integers, and left-justified, for floating point. Addition is different,
  but multiplication is the same.
-}

stripLeading0 :: (Integral a) => Seq.Seq a -> Seq.Seq a
-- Removes the leading 0 from a sequence of numbers.
-- If it's a right-justified mantissa, this has no effect on the value.
-- If it's a left-justified mantissa, this multiplies by G^digitsPerLimb.
stripLeading0 Seq.Empty = Seq.Empty
stripLeading0 (0:<|xs) = stripLeading0 xs
stripLeading0 (x:<|xs) = x<|xs

stripTrailing0 :: (Integral a) => Seq.Seq a -> Seq.Seq a
-- Removes the trailing 0 from a sequence of numbers.
-- If it's a right-justified mantissa, this divides by G^digitsPerLimb.
-- If it's a left-justified mantissa, has no effect on the value.
stripTrailing0 Seq.Empty = Seq.Empty
stripTrailing0 (xs:|>0) = stripTrailing0 xs
stripTrailing0 (xs:|>x) = xs|>x

splitLimb :: Word -> Word -> (Word,Word)
-- n is in [0..digitsPerLimb]. Values outside this range return garbage.
-- Shifts limb left by n digits, a being the more significant limb.
splitLimb n limb = (a,b) where
  x = 7 ^ n
  y = 7 ^ (digitsPerLimb-n)
  a = limb `div` y
  b = (limb `mod` y) * x

lengthenRjust :: Int -> Seq.Seq Word -> Seq.Seq Word
-- Adds zeroes or removes numbers from the start until it has the right length.
lengthenRjust n xs =
  if n > length xs
  then (Seq.replicate (n - Seq.length xs) 0) >< xs
  else Seq.drop (Seq.length xs - n) xs

shiftLSmall :: Seq.Seq Word -> Word -> Seq.Seq Word
-- n is in [0..digitsPerLimb]. Values outside this range return garbage.
-- Shifts limbs left by n. Result has one more limb.
shiftLSmall Seq.Empty _ = Seq.singleton 0
shiftLSmall (limb:<|limbs) n = splh<|(spll+res)<|ress where
  (splh,spll) = splitLimb n limb
  res:<|ress = shiftLSmall limbs n

shiftRSmall limbs n = shiftLSmall limbs (digitsPerLimb-n)

shiftLLimbs :: Integral a => Seq.Seq a -> a -> Seq.Seq a
shiftLLimbs limbs 0 = limbs
shiftLLimbs limbs n = (shiftLLimbs limbs (n-1)) |> 0

shiftRLimbs :: Integral a => Seq.Seq a -> a -> Seq.Seq a
shiftRLimbs limbs 0 = limbs
shiftRLimbs limbs n = 0 <| (shiftRLimbs limbs (n-1))

shiftLRjust :: Seq.Seq Word -> Word -> Seq.Seq Word
shiftLRjust limbs n = shiftLLimbs (shiftLSmall limbs (n `mod` digitsPerLimb)) (n `div` digitsPerLimb)

shiftRRjust :: Seq.Seq Word -> Word -> Seq.Seq Word
shiftRRjust limbs n = Seq.take (length res - (fromIntegral m)) res where
  res = shiftRLimbs (shiftRSmall limbs (n `mod` digitsPerLimb)) (n `div` digitsPerLimb)
  m = n `div` digitsPerLimb + 1

addRjust_c :: Word -> Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
addRjust_c 0 Seq.Empty ys = ys
addRjust_c 0 xs Seq.Empty = xs
addRjust_c c Seq.Empty ys = addRjust_c c (Seq.singleton 0) ys
addRjust_c c xs Seq.Empty = addRjust_c c xs (Seq.singleton 0)
addRjust_c c (xs:|>x) (ys:|>y) = (addRjust_c c1 xs ys):|>z where
  (z,c1) = addLimbs x y c

addRjust :: Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
-- Add right-justified mantissas (Eisenstein integers).
-- The result will have 0 or 1 more limb than the longer input.
addRjust = addRjust_c 0

addLjust :: Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
-- Add left-justified mantissas (floating-point numbers).
-- The result will have one more limb on the left.
addLjust a Seq.Empty = 0<|a
addLjust Seq.Empty b = 0<|b
addLjust (a:<|as) (b:<|bs) = c<|d<|es where
  e:<|es = addLjust as bs
  (d,c) = addLimbs a b e

-- Mantissa multiplication does not depend on which end they're justified on.

mulMant_pair :: Word -> Seq.Seq Word -> Seq.Seq (Word,Word)
mulMant_pair 0 _ = Seq.Empty
mulMant_pair a Seq.Empty = Seq.Empty
mulMant_pair a (bs:|>b) = (mulMant_pair a bs):|>(mulLimbs a b)

addCarriesLimb :: Word -> Word -> Seq.Seq (Word,Word) -> Seq.Seq Word
addCarriesLimb 0 0 Seq.Empty = Seq.Empty
addCarriesLimb 0 c Seq.Empty = Seq.singleton c
addCarriesLimb g 0 Seq.Empty = Seq.singleton g
addCarriesLimb g c Seq.Empty = addCarriesLimb g c (Seq.singleton (0,0))
addCarriesLimb g c (xs:|>(a,b)) = (addCarriesLimb b d xs):|>s where
  (s,d) = addLimbs a g c

mulMantLimb :: Word -> Seq.Seq Word -> Seq.Seq Word
mulMantLimb a b = addCarriesLimb 0 0 (mulMant_pair a b)

mulMantShort :: Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
mulMantShort Seq.Empty _ = Seq.Empty
mulMantShort (as:|>a) bs = addRjust (mulMantLimb a bs) ((mulMantShort as bs)|>0)

mulMant :: Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
mulMant as bs = lengthenRjust (Seq.length as + Seq.length bs) (mulMantShort as bs)
