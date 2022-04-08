module Data.GosperBase.Internals
  ( join343,mul343c,addLimbs,negateLimb,split343,
    add7,add7s,addCarries343,stripLeading0,stripTrailing0,splitLimb,negateMantissa,
    msdPosRjust,msdPosLjust )
  where
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

digitsPerLimb = 11::Word -- TODO make this depend on word size

-- |Addition table for Gosper base. In base 7:
-- 0, 1, 2, 3, 4, 5, 6
-- 1,12, 3,34, 5,16, 0
-- 2, 3,24,25, 6, 0,61
-- 3,34,25,36, 0, 1, 2
-- 4, 5, 6, 0,41,52,43
-- 5,16, 0, 1,52,53, 4
-- 6, 0,61, 2,43, 4,65
aTable=array ((0,0),(6,6))
  [ ((0,0), 0), ((0,1), 1), ((0,2), 2), ((0,3), 3), ((0,4), 4), ((0,5), 5), ((0,6), 6),
    ((1,0), 1), ((1,1), 9), ((1,2), 3), ((1,3),25), ((1,4), 5), ((1,5),13), ((1,6), 0),
    ((2,0), 2), ((2,1), 3), ((2,2),18), ((2,3),19), ((2,4), 6), ((2,5), 0), ((2,6),43),
    ((3,0), 3), ((3,1),25), ((3,2),19), ((3,3),27), ((3,4), 0), ((3,5), 1), ((3,6), 2),
    ((4,0), 4), ((4,1), 5), ((4,2), 6), ((4,3), 0), ((4,4),29), ((4,5),37), ((4,6),31),
    ((5,0), 5), ((5,1),13), ((5,2), 0), ((5,3), 1), ((5,4),37), ((5,5),38), ((5,6), 4),
    ((6,0), 6), ((6,1), 0), ((6,2),43), ((6,3), 2), ((6,4),31), ((6,5), 4), ((6,6),47)
  ] :: Array (Word8,Word8) Word8

-- |Multiplication table for Gosper base
mTable=array ((0,0),(6,6)) -- Multiplication table for Gosper base
  [ ((0,0),0), ((0,1),0), ((0,2),0), ((0,3),0), ((0,4),0), ((0,5),0), ((0,6),0),
    ((1,0),0), ((1,1),1), ((1,2),2), ((1,3),3), ((1,4),4), ((1,5),5), ((1,6),6),
    ((2,0),0), ((2,1),2), ((2,2),4), ((2,3),6), ((2,4),1), ((2,5),3), ((2,6),5),
    ((3,0),0), ((3,1),3), ((3,2),6), ((3,3),2), ((3,4),5), ((3,5),1), ((3,6),4),
    ((4,0),0), ((4,1),4), ((4,2),1), ((4,3),5), ((4,4),2), ((4,5),6), ((4,6),3),
    ((5,0),0), ((5,1),5), ((5,2),3), ((5,3),1), ((5,4),6), ((5,5),4), ((5,6),2),
    ((6,0),0), ((6,1),6), ((6,2),5), ((6,3),4), ((6,4),3), ((6,5),2), ((6,6),1)
  ] :: Array (Word8,Word8) Word8

-- |Reciprocal table for Gosper base
rTable=array (0,6)
  [ (0,0), (1,1), (2,4), (3,5), (4,2), (5,3), (6,6)
  ] :: Array Word8 Word8

-- Operations on single digits

th7dig :: (Integral a) => Int -> a -> a
th7dig pos num = (num `div` 7^pos) `mod` 7

split7 :: Word32 -> [Word32]
split7 0 = []
split7 n = (n `mod` 7) : split7 (n `div` 7)

join7 :: [Word32] -> Word32
join7 [] = 0
join7 (n:ns) = (join7 ns) * 7 + n

split343 :: Word -> [Word32]
split343 0 = []
split343 n = fromIntegral (n `mod` 343) : split343 (n `div` 343)

join7_3 :: [Word32] -> [Word32]
join7_3 [] = []
join7_3 (n0:[]) = [n0]
join7_3 (n0:n1:[]) = [7 * n1 + n0]
join7_3 (n0:n1:n2:ns) = 49*n2+7*n1+n0 : join7_3 ns

join343 :: [Word32] -> Word
join343 [] = 0
join343 (n:ns) = (fromIntegral n) + 343 * join343 ns

add7 :: Integral n => n -> n -> n -> (n,n)
-- ^Adds three Gosper base digits, returning a tuple of the sum and the carry.
-- This is a full adder.
-- a, b, c, sum3, and carry are all in [0 .. 6].
add7 a b c = (fromIntegral sum3,fromIntegral carry) where
  a8 = fromIntegral a
  b8 = fromIntegral b
  c8 = fromIntegral c
  sum2 = (aTable ! (a8,b8)) `mod` 7
  car2 = (aTable ! (a8,b8)) `div` 7
  sum3 = (aTable ! (sum2,c8)) `mod` 7
  car3 = (aTable ! (sum2,c8)) `div` 7
  carry = aTable ! (car2,car3)

add7s_c :: Integral n => n -> [n] -> [n] -> [n]
add7s_c 0 [] ys = ys
add7s_c 0 xs [] = xs
add7s_c c [] ys = add7s_c c [0] ys
add7s_c c xs [] = add7s_c c xs [0]
add7s_c c (x:xs) (y:ys) = z:add7s_c c1 xs ys where
  (z,c1) = add7 x y c

add7s :: Integral n => [n] -> [n] -> [n]
add7s = add7s_c 0

add343 :: Word32 -> Word32 -> Word32
add343 a b = join7 (add7s (split7 a) (split7 b))

neg7 :: Integral n => n -> n
-- ^Negates a digit in [0..6]. Any other argument will get a wrong answer.
neg7 0 = 0
neg7 a = 7 - a

mul7s_dig :: Integral n => n -> [n] -> [n]
mul7s_dig 0 _ = []
mul7s_dig a [] = []
mul7s_dig a (b:bs) = (fromIntegral (mTable ! (fromIntegral a,fromIntegral b))):
  mul7s_dig a bs

mul7s :: Integral n => [n] -> [n] -> [n]
mul7s [] _ = []
mul7s (a:as) bs = add7s (mul7s_dig a bs) (0:mul7s as bs)

mul343 :: Word32 -> Word32 -> Word32
mul343 a b = join7 (mul7s (split7 a) (split7 b))

recip7 :: Integral n => n -> n
-- Reciprocal of a digit. Returns 0 for 0.
recip7 a = fromIntegral (rTable ! (fromIntegral a))

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
-- ^Adds three three-digit numbers in Gosper base, returning three-digit sum and carry.
-- This is a full adder.
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
{-^
  Converts a list of (sum,carry) tuples to a list of digits in base G^3.
  The tuples result from multiplying a list by a digit.
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

plusSnd :: Integral a => a -> (b,a) -> (b,a)
plusSnd n (x,y) = (x,n+y)

negSnd :: Integral a => (b,a) -> (b,a)
negSnd (x,y) = (x,(-y))

msdPosLimb :: Word -> (Word,Word)
-- ^Returns the most significant digit and its position.
-- Given 0, returns (0,0); given 6, returns (6,1).
msdPosLimb n
  | n >= 7^8	= plusSnd 8 (msdPosLimb (n `div` 7^8))
  | n >= 7^4	= plusSnd 4 (msdPosLimb (n `div` 7^4))
  | n >= 7^2	= plusSnd 2 (msdPosLimb (n `div` 7^2))
  | n >= 7^1	= plusSnd 1 (msdPosLimb (n `div` 7^1))
  | n >= 7^0	= (n,1)
  | otherwise	= (0,0)

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
-- ^Removes the leading 0 from a sequence of numbers.
-- If it's a right-justified mantissa, this has no effect on the value.
-- If it's a left-justified mantissa, this multiplies by G^digitsPerLimb.
stripLeading0 Seq.Empty = Seq.Empty
stripLeading0 (0:<|xs) = stripLeading0 xs
stripLeading0 (x:<|xs) = x<|xs

stripTrailing0 :: (Integral a) => Seq.Seq a -> Seq.Seq a
-- ^Removes the trailing 0 from a sequence of numbers.
-- If it's a right-justified mantissa, this divides by G^digitsPerLimb.
-- If it's a left-justified mantissa, has no effect on the value.
stripTrailing0 Seq.Empty = Seq.Empty
stripTrailing0 (xs:|>0) = stripTrailing0 xs
stripTrailing0 (xs:|>x) = xs|>x

splitLimb :: Word -> Word -> (Word,Word)
-- ^n is in [0..digitsPerLimb]. Values outside this range return garbage.
-- Shifts limb left by n digits, a being the more significant limb.
splitLimb n limb = (a,b) where
  x = 7 ^ n
  y = 7 ^ (digitsPerLimb-n)
  a = limb `div` y
  b = (limb `mod` y) * x

msdPosRjust :: Seq.Seq Word -> (Word,Word)
-- ^Returns the most significant digit and its position.
-- Given 0, returns (0,0); given 64205316420531, returns (6,14).
msdPosRjust Seq.Empty = (0,0)
msdPosRjust (0:<|ns) = msdPosRjust (stripLeading0 ns)
msdPosRjust (n:<|ns) = plusSnd (digitsPerLimb * (fromIntegral (Seq.length ns))) (msdPosLimb n)

msdPosLjust :: Seq.Seq Word -> (Word,Word)
-- ^Returns the most significant digit and the number of zeroes before it.
-- Given 0, returns (0,0); given 64205316420531, returns (6,8),
-- assuming 11 or 22 digits per limb.
msdPosLjust Seq.Empty = (0,0)
msdPosLjust (0:<|ns) = plusSnd digitsPerLimb (msdPosLjust ns)
msdPosLjust (n:<|ns) = plusSnd digitsPerLimb (negSnd (msdPosLimb n))

lengthenRjust :: Int -> Seq.Seq Word -> Seq.Seq Word
-- ^Adds zeroes or removes numbers from the start until it has the right length.
lengthenRjust n xs =
  if n > length xs
  then (Seq.replicate (n - Seq.length xs) 0) >< xs
  else Seq.drop (Seq.length xs - n) xs

shiftLSmall :: Seq.Seq Word -> Word -> Seq.Seq Word
-- ^n is in [0..digitsPerLimb]. Values outside this range return garbage.
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
-- ^Add right-justified mantissas (Eisenstein integers).
-- The result will have 0 or 1 more limb than the longer input.
addRjust = addRjust_c 0

addLjust :: Seq.Seq Word -> Seq.Seq Word -> Seq.Seq Word
-- ^Add left-justified mantissas (floating-point numbers).
-- The result will have one more limb on the left.
addLjust a Seq.Empty = 0<|a
addLjust Seq.Empty b = 0<|b
addLjust (a:<|as) (b:<|bs) = c<|d<|es where
  e:<|es = addLjust as bs
  (d,c) = addLimbs a b e

negateMantissa :: Seq.Seq Word -> Seq.Seq Word
negateMantissa Seq.Empty = Seq.Empty
negateMantissa (as:|>a) = (negateMantissa as) :|> (negateLimb a)

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
