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
  [ testCase "split343 524288" $
      (split343 524288) `compare` [184,156,4] @?= EQ,
    testCase "split343 531441" $
      (split343 531441) `compare` [134,177,4] @?= EQ
  ]
