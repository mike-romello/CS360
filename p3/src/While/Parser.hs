module While.Parser where

import Parsing
    ( Alternative((<|>)),
      Parser,
      parse,
      eof,
      keyword,
      identifier,
      integer,
      symbol,
      parens,
      chainl1 )

import While.Syntax (Stm(..), Bexp(..), Aexp(..))

-- | Parse a While program.
parseWhile :: String -> Maybe Stm
parseWhile s =
    case parse p s of
      [(stm, _)] -> Just stm
      _          -> Nothing
  where
    p :: Parser Stm
    p = do r <- stmP
           eof
           return r

-- Parse an arithmetic expression
aexpP :: Parser Aexp
aexpP = term `chainl1` addop
  where
    term :: Parser Aexp
    term = factor `chainl1` mulop

    factor :: Parser Aexp
    factor =  Const <$> integer
          <|> Var <$> identifier
          <|> parens aexpP

    mulop  =  (symbol "*" >> return Mul)
          <|> (symbol "/" >> return Div)

    addop  =  (symbol "+" >> return Add)
          <|> (symbol "-" >> return Sub)

-- Parse a boolean expression
bexpP :: Parser Bexp
bexpP = term `chainl1` boolop
  where
    term :: Parser Bexp
    term =  (symbol "true" >> return BTrue)
        <|> (symbol "false" >> return BFalse)
        <|> do { _ <- symbol "~"
               ; b <- bexpP
               ; return (Not b)
               }
        <|> do { a1 <- aexpP
               ; op <- eqop
               ; a2 <- aexpP
               ; return (op a1 a2)
               }
        <|> parens bexpP

    eqop  =  (symbol "=" >> return Eq)
         <|> (symbol "<=" >> return Le)

    boolop = symbol "&&" >> return And

-- Parse a statement
stmP :: Parser Stm
stmP = term `chainl1` seqop
  where
    term :: Parser Stm
    term = factor `chainl1` orop

    factor :: Parser Stm
    factor =  do { keyword "skip"
                 ; return Skip
                 }
          <|> do { keyword "if"
              ; b <- bexpP
                ; keyword "then"
                ; s1 <- stmP
                ; keyword "else"
                ; s2 <- stmP
                ; return (If b s1 s2)
                }
         <|> do { keyword "while"
                ; b <- bexpP
                ; s <- stmP
                ; return (While b s)
                }
         <|> do { x <- identifier
                ; keyword ":="
                ; a <- aexpP
                ; return (Assign x a)
                }
         <|> parens stmP

    seqop = keyword ";" >> return Seq

    orop  =  (keyword "or"  >> return Or)
         <|> (keyword "par" >> return Or)
