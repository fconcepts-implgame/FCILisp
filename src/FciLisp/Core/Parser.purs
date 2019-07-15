module FciLisp.Core.Parser
  ( expression
  ) where

import Prelude hiding (Ordering(..), between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.List (List(..))
import Data.Maybe (maybe)
import Data.Natural (fromInt)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (sepEndBy, between)
import Text.Parsing.Parser.String (eof, oneOf, string)
import Text.Parsing.Parser.Token (space, alphaNum)
import FciLisp.Core.Interfaces.Ast (Lisp(..))

expression :: ParserT String Identity Lisp
expression = spaces *> expr <* eof
  where
  spaces = A.many $ void space

  spaces1 = A.some $ void space

  nil = const LNil <$> string "nil"

  t = const LT <$> string "t"

  natOrSymbol =
    fromCharArray <$> (A.some $ alphaNum <|> oneOf [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '=', '+', '`', '~', '|', ';', ':', '\'', '"', ',', '<', '.', '>', '/', '?' ])
      <#> \str -> fromString str # maybe (LSymbol str) (LNat <<< fromInt)

  parens = between (string "(" <* spaces) (string ")")

  list e =
    parens (sepEndBy e spaces1)
      <#> \xs -> case xs of
          Nil -> LNil
          Cons y ys -> LList y ys

  expr = fix \e -> nil <|> t <|> natOrSymbol <|> list e
