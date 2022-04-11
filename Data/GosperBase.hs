module Data.GosperBase where
import Data.Array.Unboxed
import Data.Word
import Data.GosperBase.Internals
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Char
import Data.List
import Data.Maybe

{- This computes complex numbers in base 2.5-√(-3/4), called the Gosper base
   because it is the scale factor from one Gosper island to the next bigger one.
   The digits are cyclotomic:
    2 3
   6 0 1
    4 5
   For layout of all numbers up to 3 digits, see doc/GosperBase.ps .
-}

newtype GosperInteger = GosperInteger (Seq.Seq Word) deriving (Show)

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

parseRjust :: Seq.Seq Char -> Maybe (Seq.Seq Word)
parseRjust as =
  let ns = chunkDigitsInt as
  in case ns of
    Just chunks -> traverse parseChunkRjust chunks
    Nothing -> Nothing

parse1InitTail :: (String, String) -> Maybe (GosperInteger, String)
parse1InitTail (a,b) =
  let aParse = parseRjust (Seq.fromList a)
  in case aParse of
    Just mant -> Just (GosperInteger mant,b)
    Nothing -> Nothing

parseGosperInteger :: String -> [(GosperInteger, String)]
parseGosperInteger str =
  let its = zip (inits str) (tails str)
  in catMaybes (fmap parse1InitTail its)

iAdd :: GosperInteger -> GosperInteger -> GosperInteger
iAdd (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (addRjust a b))

iMult :: GosperInteger -> GosperInteger -> GosperInteger
iMult (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (mulMant a b))
