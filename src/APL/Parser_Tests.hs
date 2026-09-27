module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Function application"
        [ parserTest "x y" $ Apply (Var "x") (Var "y"),
          parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "f 1 true" $ Apply (Apply (Var "f") (CstInt 1)) (CstBool True),
          parserTest "f\t x\n y " $ Apply (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "xy" $ Var "xy",
          parserTest "f x + y" $ Add (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "x - f y" $ Sub (Var "x") (Apply (Var "f") (Var "y")),
          parserTest "f x * g y z" $
            Mul (Apply (Var "f") (Var "x")) (Apply (Apply (Var "g") (Var "y")) (Var "z")),
          parserTest "f x / g y" $
            Div (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y")),
          parserTest "f (x + y)" $ Apply (Var "f") (Add (Var "x") (Var "y")),
          parserTest "f (if x then y else z)" $
            Apply (Var "f") (If (Var "x") (Var "y") (Var "z")),
          parserTest "(if x then f else g) y" $
            Apply (If (Var "x") (Var "f") (Var "g")) (Var "y"),
          parserTest "if f x then g y else h z" $
            If (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y")) (Apply (Var "h") (Var "z")),
          parserTestFail "x if x then y else z",
          parserTestFail "f ()",
          parserTestFail "f (x",
          parserTestFail "f 123x"
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ]
    ]
