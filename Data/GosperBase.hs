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

newtype GosperInteger = GosperInteger (Seq.Seq Word) deriving (Show)

iAdd :: GosperInteger -> GosperInteger -> GosperInteger
iAdd (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (addRjust a b))

iMult :: GosperInteger -> GosperInteger -> GosperInteger
iMult (GosperInteger a) (GosperInteger b) =
  GosperInteger (stripLeading0 (mulMant a b))
