module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

import qualified MdParser as P
import qualified MdAst as A
import Data.Either (isLeft)
import Debug.Trace (trace)

heading = runParser P.heading () ""
createExpectedHeading level title = return (A.Heading (A.HeadingLevel level) title)

main = defaultMain testSuite

trace' x = trace (show x) x

testSuite :: TestTree
testSuite = testGroup "Parsing Tests"
    [ testCase "Heading can be ended by newline" $ heading "# Test\n" @?= createExpectedHeading 1 "Test"
    , testCase "Heading can be ended by EOF" $ heading "# Test" @?= createExpectedHeading 1 "Test"
    , testCase "Heading's value can be many words" $ heading "## Some test with Many Words" @?= createExpectedHeading 2 "Some test with Many Words"
    , testCase "Heading's value is trimmed" $ heading "### Some Complex Heading " @?= createExpectedHeading 3 "Some Complex Heading"
    , testCase "Heading's level can be 6" $ heading "###### Test\n" @?= createExpectedHeading 6 "Test"
    , testCase "Heading's value must not be empty" $ assertBool "`### ` did not cause an error" $ isLeft (trace' $ heading "### ")
    , testCase "Heading's level must be less than 7 " $ assertBool "Seven `#`s did not cause an error" $ isLeft $ trace' $ heading "####### "
    ]
