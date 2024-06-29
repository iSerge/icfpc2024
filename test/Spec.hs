import Base94
import Evaluate
import Parser (parseExpr)
import Term
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
      testCase "base94 value of 15818151 is 4%34" $ n2icfp 15818151 @?= "4%34",
      testCase "Int value of I4%34 is 15818151" $ parseExpr "I4%34" @?= I 15818151,
      testCase "ICFP value of test is 4%34" $ "4%34" @?= s2icfp "test",
      testCase "Text value of B%,,/}Q/2,$_ is Hello World!" $
        icfp2s "B%,,/}Q/2,$_" @?= "Hello World!",
      -- QC.testProperty "To base94 and back"
      --   $ \n -> (n :: Int) == (icfp2n . n2icfp) n
      testCase "Evaluate int" $ eval (parseExpr "I$") @?= (Right $ I 3, 0),
      testCase "Evaluate str" $ eval (parseExpr "S4%34") @?= (Right $ S "test", 0),
      testCase "Integer negation str" $ eval (parseExpr "U- I$") @?= (Right $ I (-3), 0),
      testCase "Boolean not" $ eval (parseExpr "U! T") @?= (Right F, 0),
      testCase "string-to-int conversion" $ eval (parseExpr "U# S4%34") @?= (Right $ I 15818151, 0),
      testCase "int-to-string conversion" $ eval (parseExpr "U$ I4%34") @?= (Right $ S "test", 0),
      testCase "Integer addition" $ eval (parseExpr "B+ I# I$") @?= (Right $ I 5, 0),
      testCase "Integer substraction" $ eval (parseExpr "B- I$ I#") @?= (Right $ I 1, 0),
      testCase "Integer multiplication" $ eval (parseExpr "B* I$ I#") @?= (Right $ I 6, 0),
      testCase "Integer division" $ eval (parseExpr "B/ U- I( I#") @?= (Right $ I (-3), 0),
      testCase "Integer modulo" $ eval (parseExpr "B% U- I( I#") @?= (Right $ I (-1), 0),
      testCase "Integer comparison" $ eval (parseExpr "B< I$ I#") @?= (Right F, 0),
      testCase "Integer comparison" $ eval (parseExpr "B> I$ I#") @?= (Right T, 0),
      testCase "Equality comparison" $ eval (parseExpr "B= I$ I#") @?= (Right F, 0),
      testCase "Equality comparison" $ eval (parseExpr "B= S$ S$") @?= (Right T, 0),
      testCase "Equality comparison" $ eval (parseExpr "B= F T") @?= (Right F, 0),
      testCase "Boolean or" $ eval (parseExpr "B| T F") @?= (Right T, 0),
      testCase "Boolean and" $ eval (parseExpr "B& T F") @?= (Right F, 0),
      testCase "Evaluate Concat" $ eval (parseExpr "B. S4% S34") @?= (Right $ S "test", 0),
      testCase "String take" $ eval (parseExpr "BT I$ S4%34") @?= (Right $ S "tes", 0),
      testCase "String drop" $ eval (parseExpr "BD I$ S4%34") @?= (Right $ S "t", 0),
      testCase "Evaluate application" $ eval (parseExpr "B$ L$ B. S4% v$ S34") @?= (Right $ S "test", 1),
      testCase "Evaluate test expr" $ eval (parseExpr "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK") @?= (Right $ S "Hello World!", 2),
      testCase "Evaluate conditional" $ eval (parseExpr "? B> I# I$ S9%3 S./") @?= (Right $ S "no", 0),
      testCase "Simple lambda abstractions" $ eval (parseExpr "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8") @?= (Right $ I 12, 2),
      testCase "Test evaluation" $ eval (parseExpr "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%") @?= (Right $ I 16, 109)
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
