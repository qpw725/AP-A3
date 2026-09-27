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
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          --Added tests Task2
          parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x==y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          --Added tests Task2
          parserTest "x*y**z" $
            Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x**y*z" $
            Mul (Pow (Var "x") (Var "y")) (Var "z"),
          parserTest "x**y**z" $
            Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y**z" $
            Add (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x==y**z" $
            Eql (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x+y==y+x" $
            Eql
              (Add (Var "x") (Var "y"))
              (Add (Var "y") (Var "x")),
          parserTest "x==y==z" $
            Eql
              (Eql (Var "x") (Var "y"))
              (Var "z")
          --Added tests Task2
        ],
      testGroup
        "Function application"
        [ parserTest "x y" $ Apply (Var "x") (Var "y"),
          parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x(y z)" $ Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTest "f x * g y z" $
            Mul (Apply (Var "f") (Var "x")) (Apply (Apply (Var "g") (Var "y")) (Var "z")),
          parserTest "f (if x then y else z)" $
            Apply (Var "f") (If (Var "x") (Var "y") (Var "z")),
          parserTestFail "x if x then y else z"
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
      --Added tests Task4
      testGroup
        "Lambda expressions"
        [ parserTest "\\x -> x" $ Lambda "x" (Var "x"),
          parserTest "\\x -> x+y*z" $
            Lambda 
              "x"
              (Add
                (Var "x")
                (Mul (Var "y") (Var "z"))),
          parserTest "(\\x -> x)+x" $
            Add
              (Lambda "x" (Var "x"))
              (Var "x"),
          parserTest "\\x -> x+y*z==w**2" $
            Lambda
              "x"
              (Eql
                (Add
                  (Var "x")
                  (Mul (Var "y") (Var "z")))
                (Pow (Var "w") (CstInt 2))),
          parserTestFail "\\ -> x",
          parserTestFail "\\x x",
          parserTestFail "\\x ->"
        ],
      testGroup
        "Let expressions"
        [ parserTest "let x = y in z" $ Let "x" (Var "y") (Var "z"),
          parserTest "let x = y+1 in x*2" $
            Let
              "x"
              (Add (Var "y") (CstInt 1))
              (Mul (Var "x") (CstInt 2)),
          parserTest "let x = let y = 1 in y in x" $
            Let
              "x"
              (Let "y" (CstInt 1) (Var "y"))
              (Var "x"),
          parserTest "let x = try y catch z in x" $
            Let
              "x"
              (TryCatch (Var "y") (Var "z"))
              (Var "x"),
          parserTest "let x = y in x+z*w==q**2" $
            Let
              "x"
              (Var "y")
              (Eql
                (Add
                  (Var "x")
                  (Mul (Var "z") (Var "w")))
                (Pow (Var "q") (CstInt 2))),
          parserTestFail "let true = y in z",
          parserTestFail "let x = y",
          parserTestFail "x let v = 2 in v"
        ],
      testGroup
        "Try-catch expressions"
        [ parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try x/0 catch y+1" $
            TryCatch
              (Div (Var "x") (CstInt 0))
              (Add (Var "y") (CstInt 1)),
          parserTest "try try x catch y catch z" $
            TryCatch
              (TryCatch (Var "x") (Var "y"))
              (Var "z"),
          parserTest "try x catch y+z*w" $
            TryCatch
              (Var "x")
              (Add
                (Var "y")
                (Mul (Var "z") (Var "w"))),
          parserTestFail "try x",
          parserTestFail "try catch y",
          parserTestFail "try in catch y",
          parserTestFail "try x catch"
        ],
      testGroup
        "Loop expressions"
        [ parserTest "loop x = 0 for i < 10 do x" $
            ForLoop
              ("x", CstInt 0)
              ("i", CstInt 10)
              (Var "x"),
          parserTest "loop x = y+1 for i < z*2 do x+i" $
            ForLoop
              ("x", Add (Var "y") (CstInt 1))
              ("i", Mul (Var "z") (CstInt 2))
              (Add (Var "x") (Var "i")),
          parserTest "loop x = 0 for i < 10 do x+i*2==y**z" $
            ForLoop
              ("x", CstInt 0)
              ("i", CstInt 10)
              (Eql
                (Add
                  (Var "x")
                  (Mul (Var "i") (CstInt 2)))
                (Pow (Var "y") (Var "z"))),
          parserTestFail "loop x = 0 for i < 10",
          parserTestFail "loop x = 0 i < 10 do x",
          parserTestFail "loop x = 0 for 10 do x"
        ],
      testGroup
        "Nested different productions"
        [
          parserTest "\\x -> try x catch y" $
            Lambda
              "x"
              (TryCatch (Var "x") (Var "y")),
          parserTest "try let x = y in x catch z" $
            TryCatch
              (Let "x" (Var "y") (Var "x"))
              (Var "z"),
          parserTest "let x = y in try x catch z" $
            Let
              "x"
              (Var "y")
              (TryCatch (Var "x") (Var "z")),
          parserTest "loop x = 0 for i < 10 do let y = i in x+y" $
            ForLoop
              ("x", CstInt 0)
              ("i", CstInt 10)
              (Let
                "y"
                (Var "i")
                (Add (Var "x") (Var "y"))),
          parserTest "\\x -> let y = x in try y catch z" $
            Lambda
              "x"
              (Let
                "y"
                (Var "x")
                (TryCatch (Var "y") (Var "z")))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ]
    ]
