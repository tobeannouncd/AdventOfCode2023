module TextParsing
  ( lexer
  , module Text.Parsec.Text
  , module Text.Parsec
  , text
  ) where
import Text.Parsec.Token ( GenLanguageDef(..), makeTokenParser, GenTokenParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Text.Parsec.Text
import Text.Parsec

emptyDefT :: GenLanguageDef Text u Identity
emptyDefT = LanguageDef
  { commentStart   = ""
  , commentEnd     = ""
  , commentLine    = ""
  , nestedComments = True
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = opLetter emptyDefT
  , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames= []
  , reservedNames  = []
  , caseSensitive  = True
  }

lexer :: GenTokenParser Text u Identity
lexer = makeTokenParser emptyDefT

text :: Text -> Parser Text
text t =
  if T.null t then return t else do
    x <- char (T.head t)
    T.cons x <$> text (T.tail t)
