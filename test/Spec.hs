import Base94
-- import Data.List
-- import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-- import Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [base94]

base94 :: TestTree
base94 =
  testGroup
    "Base94 conversions"
    [ testCase "Numeric value of $ is 3" $ icfp2n "$" @?= 3,
      testCase "Numeric value of # is 2" $ icfp2n "#" @?= 2,
      testCase "Text value of 4%34 is test" $ icfp2s "4%34" @?= "test",
      testCase "ICFP value of test is 4%34" $ "4%34" @?= s2icfp "test",
      testCase "Text value of B%,,/}Q/2,$_ is Hello World!" $
        icfp2s "B%,,/}Q/2,$_" @?= "Hello World!"
      -- QC.testProperty "To base94 and back"
      --   $ \n -> (n :: Int) == (icfp2n . n2icfp) n
    ]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]
--
-- scProps :: TestTree
-- scProps =
--   testGroup
--     "(checked by SmallCheck)"
--     [ SC.testProperty "sort == sort . reverse" $
--         \list -> sort (list :: [Int]) == sort (reverse list),
--       SC.testProperty "Fermat's little theorem" $
--         \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
--       -- the following property does not hold
--       SC.testProperty "Fermat's last theorem" $
--         \x y z n ->
--           (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
--     ]
--
-- qcProps :: TestTree
-- qcProps =
--   testGroup
--     "(checked by QuickCheck)"
--     [ QC.testProperty "sort == sort . reverse" $
--         \list -> sort (list :: [Int]) == sort (reverse list),
--       QC.testProperty "Fermat's little theorem" $
--         \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0,
--       -- the following property does not hold
--       QC.testProperty "Fermat's last theorem" $
--         \x y z n ->
--           (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
--     ]
--
-- unitTests :: TestTree
-- unitTests =
--   testGroup
--     "Unit tests"
--     [ testCase "List comparison (different length)" $
--         [1, 2, 3] `compare` [1, 2] @?= GT,
--       -- the following test does not hold
--       testCase "List comparison (same length)" $
--         [1, 2, 3] `compare` [1, 2, 2] @?= LT
--     ]
