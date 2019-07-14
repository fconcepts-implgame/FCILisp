module FCILisp.Core.Parser
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
import FciLisp.Core.Ast (Lisp(..))

expression :: ParserT String Identity Lisp
expression = spaces *> expr <* eof
  where
  sep = void space

  spaces = A.many sep

  spaces1 = A.some sep

  nil = const LNil <$> string "nil"

  t = const LT <$> string "t"

  symbol =
    fromCharArray <$> (A.some $ alphaNum <|> oneOf [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '=', '+', '`', '~', '|', ';', ':', '\'', '"', ',', '<', '.', '>', '/', '?' ])
      >>= \str -> pure $ fromString str # maybe (LSymbol str) (LNat <<< fromInt)

  parens = between (string "(" <* spaces) (spaces *> string ")")

  list e =
    parens ((sepEndBy e spaces1) <* spaces)
      >>= \xs -> case xs of
          Nil -> pure LNil
          Cons y ys -> pure $ LList y ys

  expr = fix \e -> nil <|> t <|> symbol <|> list e
