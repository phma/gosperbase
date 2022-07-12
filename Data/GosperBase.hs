module Data.GosperBase where
import Data.Array
import Data.Word
import Data.GosperBase.Internals
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import qualified Math.NumberTheory.Quadratic.EisensteinIntegers as Eis
import Data.Char
import Data.List
import Data.Bits
import Data.Maybe

{- This computes complex numbers in base 2.5-√(-3/4), called the Gosper base
   because it is the scale factor from one Gosper island to the next bigger one.
   The digits are cyclotomic:
    2 3
   6 0 1
    4 5
   For layout of all numbers up to 3 digits, see doc/GosperBase.ps .
-}

-- The argument of G as a fraction of 60°, expressed as a rational approximation.
-- -0.318443422514484906575291245669
-- -[0;3,7,7,1,3,5,5,2,3,9,2,1,2,6,1,12,2,1,6,2,1,28,3,11,25]
argNum = -536627952198047
argDenom = 1685159479698902

tomicTable = array (0,5)
  [(0,1),(1,3),(2,2),(3,6),(4,4),(5,5)]
  :: Array Word Word

argBasePower :: Integral a => a -> Word
-- ^Returns the digit whose argument is closest to that of the nth power of the base.
-- E.g. argBasePower 3 == 5 because 1000G's argument is -57.32°
-- and 5G's argument is -60°.
argBasePower n = tomicTable ! (fromIntegral (mod rot 6)) where
  rot = ((fromIntegral n) * argNum + argDenom `div` 2) `div` argDenom

newtype GosperInteger = GosperInteger (Seq.Seq Word) deriving (Eq)

qdSign :: GosperInteger -> Int
-- If n is real, returns its correct sign. If n is within 30° of the imaginary
-- axis, returns +1 or -1 depending on its magnitude. Two "positive" numbers
-- can add up to a "negative" one, or vice versa, hence qd = quick and dirty.
qdSign (GosperInteger n) = s where
  (msd,pos) = msdPosRjust n
  msdRot = (fromIntegral msd) `mul343` (fromIntegral (argBasePower (pos-1)))
  s = if msdRot == 0 then 0 else
    if (msdRot .&. 1) == 1 then 1 else -1

gosperSerial :: GosperInteger -> Integer
-- ^Reinterprets the base-G representation as base 7.
-- E.g. 2 is expressed as 12 in base G; 12 in base 7 means 9.
gosperSerial (GosperInteger a) = mantissaSerial a

chunkDigitsInt :: Seq.Seq Char -> Maybe (Seq.Seq (Seq.Seq Char))
-- ^If the string ends in 'G', reverses the rest of the characters
-- and groups them into chunks of digitsPerLimb.
chunkDigitsInt (as:|>'G') = Just (Seq.reverse (Seq.chunksOf (fromIntegral digitsPerLimb) (Seq.reverse as)))
chunkDigitsInt as = Nothing

parseChunkRjust :: Seq.Seq Char -> Maybe Word
parseChunkRjust Seq.Empty = Just 0
parseChunkRjust (n:<|ns) =
  let ms = parseChunkRjust ns
  in case ms of
    Just num -> if (n >= '0' && n < '7')
		   then Just (7 * num + fromIntegral (ord n - ord '0'))
		   else Nothing
    Nothing -> Nothing

showLimb :: Word -> Word -> String
showLimb _ 0 = ""
showLimb val ndig = chr (fromIntegral ((val `div` 7 ^ (ndig-1)) `mod` 7) + ord '0') : (showLimb val (ndig-1))

parseRjust :: Seq.Seq Char -> Maybe (Seq.Seq Word)
parseRjust as =
  let ns = chunkDigitsInt as
  in case ns of
    Just chunks -> traverse parseChunkRjust chunks
    Nothing -> Nothing

showRjust' :: Seq.Seq Word -> String
showRjust' Seq.Empty = ""
showRjust' (a:<|as) = (showLimb a digitsPerLimb) ++ (showRjust' as)

showRjust :: Seq.Seq Word -> String
showRjust Seq.Empty = "0"
showRjust (a:<|as) = (showLimb a (snd (msdPosLimb a))) ++ (showRjust' as)

parse1InitTail :: (String, String) -> Maybe (GosperInteger, String)
parse1InitTail (a,b) =
  let aParse = parseRjust (Seq.fromList a)
  in case aParse of
    Just mant -> Just (GosperInteger mant,b)
    Nothing -> Nothing

parseGosperInteger :: String -> [(GosperInteger, String)]
parseGosperInteger str =
  let its = zip (inits str) (tails str) -- TODO stop on invalid char
  in catMaybes (fmap parse1InitTail its)

instance Read GosperInteger where
  readsPrec _ str = parseGosperInteger str

instance Show GosperInteger where
  show (GosperInteger m) = showRjust m ++ "G"

iAdd :: GosperInteger -> GosperInteger -> GosperInteger
iAdd (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (addRjust a b))

iMult :: GosperInteger -> GosperInteger -> GosperInteger
iMult (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (mulMant a b))

iNegate :: GosperInteger -> GosperInteger
iNegate (GosperInteger a) = GosperInteger (negateMantissa a)

iSub :: GosperInteger -> GosperInteger -> GosperInteger
iSub a b = iAdd a (iNegate b)

iSignum :: GosperInteger -> GosperInteger
iSignum a =
  let n1 = normGosper (iSub a (read "1G"))
      n2 = normGosper (iSub a (read "2G"))
      n3 = normGosper (iSub a (read "3G"))
      n4 = normGosper (iSub a (read "4G"))
      n5 = normGosper (iSub a (read "5G"))
      n6 = normGosper (iSub a (read "6G"))
  in case compare n1 n6 of
    LT -> read "1G"
    EQ -> read "0G"
    GT -> read "6G"

instance Ord GosperInteger where
  -- compare can give circular ordering of purely imaginary numbers.
  compare a b = compare (qdSign (iSub a b)) 0

-- Define constants used in conversion

zero = read("0G") :: GosperInteger
one = read("1G") :: GosperInteger
ω = read("2G") :: GosperInteger
naturals = 0 : map (+1) naturals
gnaturals = zero : map (iAdd one) gnaturals
g256 = gnaturals !! 256

byteTable=array (0,255) (take 256 (zip naturals gnaturals))

integerToGosperInteger :: Integer -> GosperInteger
integerToGosperInteger a
  | a < 0     = iNegate (integerToGosperInteger (- a))
  | a < 256   = byteTable ! a
  | otherwise = iAdd (byteTable ! (a `mod` 256))
		     (iMult g256 (integerToGosperInteger (a `div` 256)))

conjInt :: GosperInteger -> GosperInteger
conjInt (GosperInteger a) = GosperInteger (conjMantRjust a)

gosperIntegerToEisensteinInteger :: GosperInteger -> Eis.EisensteinInteger
gosperIntegerToEisensteinInteger (GosperInteger a) = eisMantissa a

normGosper :: GosperInteger -> GosperInteger
normGosper a = a `iMult` (conjInt a)

-- Normal exponent mantissa; the mantissa is left-justified and starts
-- after the base point.
data BareGosperFloat = Normal Int (Seq.Seq Word) | Inf | Nan deriving Show

normalize :: BareGosperFloat -> BareGosperFloat
normalize Inf = Inf
normalize Nan = Nan
normalize (Normal exp mant) =
  if nexp > maxExp
  then Inf
  else Normal nexp nmant
  where
    nzeros0 = fromIntegral (snd (msdPosLjust mant))
    nzeros = if exp - nzeros0 < minExp then exp - minExp else nzeros0
    nexp = exp - nzeros
    nmant = shiftLLjust mant (fromIntegral nzeros)

