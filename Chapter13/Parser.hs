-- The parser that will be defined below is 
-- taken from Graham Hutton's Programming in Haskell book.
-- I followed the chapter pages to reproduce the exact same
-- parser that he provides on the online version of the book.

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x, xs)])

-- Making the Parser a Functor
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                              []         -> []
                              [(v, out)] -> [(g v, out)])

-- Making the Parser an Applicative
instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of 
                      []         -> []
                      [(g, out)] -> parse (fmap g px) out)

-- Making the Parser a Monad
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> parse (f v) out)

-- Making the Parser an Alternative
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                            []         -> parse q inp
                            [(v, out)] -> [(v, out)])

-- Derived primitives
-- ------------------

sat :: (Char -> Bool) -> Parser Char
sat p  = do x <- item
            if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing
-------------------

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Parser for a non-empty list of natural numbers that ignores spacing
nats :: Parser [Int]
nats = do symbol "["
          n  <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

-- Arithmetic expressions
-- ----------------------

-- expr   ::= term (+ expr | empty)
-- term   ::= factor (* term | empty)
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 | ...

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             e <- expr
             return (f * e)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

-- Evaluating an expression
eval :: String -> Int
eval xs = case (parse expr xs) of
             [(n, [])]  -> n
             [(_, out)] -> error ("Unused input " ++ out)
             []         -> error ("Invalid input")
