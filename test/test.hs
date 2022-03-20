{-# LANGUAGE InstanceSigs #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Word
import Data.GosperBase
import qualified Data.Sequence as Seq

main = defaultMain tests

newtype Digit7 = Digit7 Int deriving Show

instance Arbitrary Digit7 where
  arbitrary :: Gen Digit7
  arbitrary = Digit7 <$> chooseInt (0,6)

newtype Digit343 = Digit343 Int deriving Show

instance Arbitrary Digit343 where
  arbitrary :: Gen Digit343
  arbitrary = Digit343 <$> chooseInt (0,342)

newtype Limb = Limb Word32 deriving Show

instance Arbitrary Limb where
  arbitrary :: Gen Limb
  arbitrary = Limb <$> choose (0,1977326742)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "split-join343" $
      \x -> join343 (split343 (x :: Word32)) == x,
    QC.testProperty "add7 is commutative 2" $
      \(Digit7 a) (Digit7 b) (Digit7 c) -> add7 a b c == add7 c b a,
    QC.testProperty "add7 is commutative 3" $
      \(Digit7 a) (Digit7 b) (Digit7 c) -> add7 a b c == add7 b c a,
    QC.testProperty "mul343c is commutative" $
      \(Digit343 a) (Digit343 b) -> mul343c a b == mul343c b a,
    QC.testProperty "limb additive inverse" $
      \(Limb a) -> addLimbs a (negateLimb a) 0 == (0,0)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "split343 0" $
      (split343 0) `compare` [] @?= EQ,
    testCase "split343 1" $
      (split343 1) `compare` [1] @?= EQ,
    testCase "split343 342" $
      (split343 342) `compare` [342] @?= EQ,
    testCase "split343 343" $
      (split343 343) `compare` [0,1] @?= EQ,
    testCase "split343 524288" $
      (split343 524288) `compare` [184,156,4] @?= EQ,
    testCase "split343 531441" $
      (split343 531441) `compare` [134,177,4] @?= EQ,
    testCase "split343 129140163" $
      (split343 129140163) `compare` [320,230,68,3] @?= EQ,
    testCase "split343 134217728" $
      (split343 134217728) `compare` [113,285,111,3] @?= EQ,
    testCase "add7 0 0 0" $
      (add7 0 0 0) `compare` (0,0) @?= EQ,
    testCase "add7 1 6 0" $
      (add7 1 6 0) `compare` (0,0) @?= EQ,
    testCase "add7 2 5 0" $
      (add7 2 5 0) `compare` (0,0) @?= EQ,
    testCase "add7 3 4 0" $
      (add7 3 4 0) `compare` (0,0) @?= EQ,
    testCase "add7 1 1 1" $
      (add7 1 1 1) `compare` (3,1) @?= EQ,
    testCase "stripLeading0 [0,1,2,3,4,5,6,0]" $
      (stripLeading0 (Seq.fromList [0,1,2,3,4,5,6,0])) `compare`
      (Seq.fromList [1,2,3,4,5,6,0]) @?= EQ,
    testCase "stripTrailing0 [0,1,2,3,4,5,6,0]" $
      (stripTrailing0 (Seq.fromList [0,1,2,3,4,5,6,0])) `compare`
      (Seq.fromList [0,1,2,3,4,5,6]) @?= EQ,
    testCase "stripLeading0 [1,2,3,4,5,6]" $
      (stripLeading0 (Seq.fromList [1,2,3,4,5,6])) `compare`
      (Seq.fromList [1,2,3,4,5,6]) @?= EQ,
    testCase "stripTrailing0 [1,2,3,4,5,6]" $
      (stripTrailing0 (Seq.fromList [1,2,3,4,5,6])) `compare`
      (Seq.fromList [1,2,3,4,5,6]) @?= EQ
  ]
