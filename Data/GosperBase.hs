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

-- Operations on groups of three and six digits
-- The maximum sum is 6566 in base 7; the maximum product is 656543 in base 7.

aTable343=array ((0,0),(342,342))
  [((fromIntegral x,fromIntegral y),fromIntegral (add343 (fromIntegral x) (fromIntegral y))) | x<-[0..342], y<-[0..342]]
  :: Array (Word16,Word16) Word16

mTable343=array ((0,0),(342,342))
  [((fromIntegral x,fromIntegral y),mul343 (fromIntegral x) (fromIntegral y)) | x<-[0..342], y<-[0..342]]
  :: Array (Word16,Word16) Word32

negTable=array (0,117648)
  [(fromIntegral x,join7 (map neg7 (split7 x))) | x<-[0..117648]]
  :: Array Word32 Word32

add343c :: Integral n => n -> n -> n -> (n,n)
-- a, b, c, sum3, and carry are all in [0 .. 342].
add343c a b c = (fromIntegral sum3,fromIntegral carry) where
  a16 = fromIntegral a
  b16 = fromIntegral b
  c16 = fromIntegral c
  sum2 = (aTable343 ! (a16,b16)) `mod` 343
  car2 = (aTable343 ! (a16,b16)) `div` 343
  sum3 = (aTable343 ! (sum2,c16)) `mod` 343
  car3 = (aTable343 ! (sum2,c16)) `div` 343
  carry = aTable343 ! (car2,car3)

add343s_c :: Integral n => n -> [n] -> [n] -> [n]
add343s_c 0 [] ys = ys
add343s_c 0 xs [] = xs
add343s_c c [] ys = add343s_c c [0] ys
add343s_c c xs [] = add343s_c c xs [0]
add343s_c c (x:xs) (y:ys) = z:add343s_c c1 xs ys where
  (z,c1) = add343c x y c

add343s :: Integral n => [n] -> [n] -> [n]
add343s = add343s_c 0

addCarries343 :: Integral n => n -> n -> [(n,n)] -> [n]
{-
  Example (in decimal): 8*16384 is [(2,3),(4,6),(4,2),(8,4),(8,0)]
  0 0 [(2,3),(4,6),(4,2),(8,4),(8,0)] -> s=2 d=0
    3 0 [(4,6),(4,2),(8,4),(8,0)] -> s=7 d=0
      6 0 [(4,2),(8,4),(8,0)] -> s=0 d=1
	2 1 [(8,4),(8,0)] -> s=1 d=1
	  4 1 [(8,0)] -> s=3 d=1
	    0 1 []
	    [1]
	  [3,1]
	[1,3,1]
      [0,1,3,1]
    [7,0,1,3,1]
  [2,7,0,1,3,1] which is 131072.
-}
addCarries343 0 0 [] = []
addCarries343 0 c [] = [c]
addCarries343 g 0 [] = [g]
addCarries343 g c [] = addCarries343 g c [(0,0)]
addCarries343 g c ((a,b):xs) = s:addCarries343 b d xs where
  (s,d) = add343c a g c

mul343c :: Integral n => n -> n -> (n,n)
mul343c a b = (fromIntegral p,fromIntegral c) where
  a16 = fromIntegral a
  b16 = fromIntegral b
  p = (mTable343 ! (a16,b16)) `mod` 343
  c = (mTable343 ! (a16,b16)) `div` 343

mul343s_pair :: Integral n => n -> [n] -> [(n,n)]
mul343s_pair 0 _ = []
mul343s_pair a [] = []
mul343s_pair a (b:bs) = (mul343c a b):mul343s_pair a bs

mul343s_dig :: Integral n => n -> [n] -> [n]
mul343s_dig a b = addCarries343 0 0 (mul343s_pair a b)

mul343s :: Integral n => [n] -> [n] -> [n]
mul343s [] _ = []
mul343s (a:as) bs = add343s (mul343s_dig a bs) (0:mul343s as bs)

-- Operations on limbs (groups of eleven digits)

negateLimb :: Word -> Word -- TODO make work when digitsPerLimb=22
negateLimb a = 117649 * b + c where
  b = fromIntegral (negTable ! (fromIntegral (a `div` 117649)))
  c = fromIntegral (negTable ! (fromIntegral (a `mod` 117649)))

addLimbs :: Word -> Word -> Word -> (Word,Word)
addLimbs a b c = (sum3,carry) where
  sums = (add343s (add343s (split343 a) (split343 b)) (split343 c)) ++ [0,0,0,0]
  sum3l = take 3 sums ++ [(sums !! 3) `mod` 49]
  carryl = (sums !! 3) `div` 49 : (drop 4 sums)
  sum3 = join343 sum3l
  carry = join343 carryl

mulLimbs :: Word -> Word -> (Word,Word)
mulLimbs a b = (prod2,carry) where
  prodl = mul343s (split343 a) (split343 b) ++ [0,0,0,0]
  prod2l = take 3 prodl ++ [(prodl !! 3) `mod` 49]
  carryl = (prodl !! 3) `div` 49 : (drop 4 prodl)
  prod2 = join343 prod2l
  carry = join343 carryl

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
