import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Word
import Data.FlowsnakeBase

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0,
    QC.testProperty "split-join343" $
      \x -> join343 (split343 (x :: Word32)) == x
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
      (split343 134217728) `compare` [113,285,111,3] @?= EQ
  ]
