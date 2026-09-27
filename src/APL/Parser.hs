module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "try",
    "catch",
    "let",
    "in",
    "loop",
    "for",
    "do"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]

pLambda :: Parser Exp
pLambda = do
  lString "\\"
  v <- lVName
  lString "->"
  e <- pExp
  pure $ Lambda v e

pTryCatch :: Parser Exp
pTryCatch = do
  lKeyword "try"
  e1 <- pExp
  lKeyword "catch"
  e2 <- pExp
  pure $ TryCatch e1 e2

pLet :: Parser Exp
pLet = do
  lKeyword "let"
  v <- lVName
  lString "="
  e1 <- pExp
  lKeyword "in"
  e2 <- pExp
  pure $ Let v e1 e2

pLoop :: Parser Exp
pLoop = do
  lKeyword "loop"
  v1 <- lVName
  lString "="
  e1 <- pExp
  lKeyword "for"
  v2 <- lVName
  lString "<"
  e2 <- pExp
  lKeyword "do"
  e3 <- pExp
  pure $ ForLoop (v1, e1) (v2, e2) e3

pLExp :: Parser Exp
pLExp =
  choice
    [ pLambda,
      pTryCatch,
      pLet,
      pLoop,
      If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      pFExp
    ]

pExp2 :: Parser Exp
pExp2 = do
  x <- pLExp
  choice
    [ do
        lString "**"
        y <- pExp2
        pure $ Pow x y,
      pure x
    ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp0
            chain $ Eql x y,
          pure x
        ]

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
