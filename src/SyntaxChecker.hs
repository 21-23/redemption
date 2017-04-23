{-# LANGUAGE OverloadedStrings #-}

module SyntaxChecker where

import Data.Text (Text)
import qualified Data.Text as Text

data Bracket
  = Parenthesis
  | Square
  | Curly deriving (Eq)

data Quote
  = Single
  | Double deriving (Eq)

data Token
  = Opening Bracket
  | Closing Bracket
  | StringDelimiter Quote
  | EscapeSlash deriving (Eq)

tokenFromChar :: Char -> Maybe Token
tokenFromChar '('  = Just $ Opening Parenthesis
tokenFromChar ')'  = Just $ Closing Parenthesis
tokenFromChar '['  = Just $ Opening Square
tokenFromChar ']'  = Just $ Closing Square
tokenFromChar '{'  = Just $ Opening Curly
tokenFromChar '}'  = Just $ Closing Curly
tokenFromChar '\'' = Just $ StringDelimiter Single
tokenFromChar '\"' = Just $ StringDelimiter Double
tokenFromChar '\\' = Just $ EscapeSlash
tokenFromChar _    = Nothing

tokenToChar :: Token -> Char
tokenToChar (Opening Parenthesis)    = '('
tokenToChar (Closing Parenthesis)    = ')'
tokenToChar (Opening Square)         = '['
tokenToChar (Closing Square)         = ']'
tokenToChar (Opening Curly)          = '{'
tokenToChar (Closing Curly)          = '}'
tokenToChar (StringDelimiter Single) = '\''
tokenToChar (StringDelimiter Double) = '\"'
tokenToChar  EscapeSlash             = '\\'

balance :: Text -> [Token]
balance string =
  Text.foldl (\s c -> maybe s (f s) (tokenFromChar c)) [] string
    where
      f (EscapeSlash:xs)           _                           = xs
      f ((Opening a):xs)          (Closing b)         | a == b = xs
      f ((StringDelimiter a):xs)  (StringDelimiter b) | a == b = xs
      f s@((StringDelimiter _):_)  EscapeSlash                 = EscapeSlash : s
      f s                          EscapeSlash                 = s
      f s@((Opening _):_)          x                           = x : s
      f s@((Closing _):_)          x                           = x : s
      f s@((StringDelimiter _):_)  _                           = s
      f []                         x                           = [x]

checkSyntax :: Text -> Either Char Text
checkSyntax solution =
  case balance solution of
    []  -> Right solution
    x:_ -> Left $ tokenToChar x
