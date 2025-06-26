{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser where

import Prelude hiding (fail)
import Data.Char
import Data.List
import Control.Monad.Trans.State

type Parser a = StateT String (Either String) a

parse = runStateT

fail :: String -> Parser a
fail s = StateT $ \_ -> Left s

char :: Parser Char
char = StateT $ \s -> case s of ""     -> Left "Unexpected end of input"
                                (c:cs) -> Right (c,cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do  c <- char
            if p c
            then return c
            else fail $ "unexpected" ++ show c

string :: String -> Parser String
string "" = return ""
string (x:xs) = do sat (== x)
                   string xs
                   return (x:xs)

infixr 2 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = StateT $ \s -> case runStateT p s of
                           Left _ -> runStateT q s
                           Right x -> Right x

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p
                 xs <- oneOrMore p <|> return []
                 return $ x:xs

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> return []

int :: Parser Int
int = do cs <- oneOrMore (sat isDigit)
         return (read cs)

space :: Parser ()
space = do _ <- oneOrMore (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do zeroOrMore $ sat isSpace
             x <- p
             zeroOrMore $ sat isSpace
             return x

symbol :: String -> Parser String
symbol s = token (string s)

oneOf :: [String] -> Parser String
oneOf strs = token $ foldr ((<|>) . tryParse) errParsers strs
  where
    tryParse = string
    errParsers = fail ("Expected one of: " ++ intercalate "," strs)

identifier :: Parser String
identifier = token $ do c <- sat isAlpha
                        cs <- zeroOrMore (sat isAlphaNum)
                        return (c:cs)
            <|> fail "Expected an identifier"
