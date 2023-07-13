{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Sirius.Parser.Lexer where

import           Data.Char
import           Language.Sirius.CST.Modules.Located
import qualified Text.Parsec                         as P
import qualified Text.Parsec.Token                   as Token

type Sirius m a = Parser m (Located a)

type Parser m a = P.ParsecT Text () m a

reservedWords :: [String]
reservedWords =
  [ "let"
  , "ref"
  , "struct"
  , "if"
  , "else"
  , "while"
  , "for"
  , "in"
  , "return"
  , "true"
  , "false"
  , "import"
  , "type"
  , "fn"
  , "property"
  , "with"
  , "to"
  , "enum"
  , "match"
  , "mod"
  ]

languageDef :: Monad m => Token.GenLanguageDef Text u m
languageDef =
  Token.LanguageDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.nestedComments = True
    , Token.caseSensitive = True
    , Token.identStart = P.letter <|> P.char '_'
    , Token.opLetter = P.oneOf ":!#$%&*+/<=>?@\\^|-~"
    , Token.opStart = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.identLetter = P.alphaNum <|> P.char '_' <|> P.char '\''
    , Token.reservedNames = reservedWords
    , Token.reservedOpNames =
        ["(", ")", "{", "}", "[", "]", ".", "->", "=>"]
    }

lexer :: Monad m => Token.GenTokenParser Text u m
lexer = Token.makeTokenParser languageDef

operator :: Monad m => Parser m Text
operator = fromString <$> Token.operator lexer

identifier :: Monad m => Parser m Text
identifier = fromString <$> Token.identifier lexer

reserved :: Monad m => String -> Parser m ()
reserved = Token.reserved lexer

reservedOp :: Monad m => String -> Parser m ()
reservedOp = Token.reservedOp lexer

parens :: Monad m => Parser m a -> Parser m a
parens = Token.parens lexer

charLiteral :: Monad m => Parser m Char
charLiteral = Token.charLiteral lexer

integer :: Monad m => Parser m Integer
integer = Token.integer lexer

float :: Monad m => Parser m Double
float = Token.float lexer

whiteSpace :: Monad m => Parser m ()
whiteSpace = Token.whiteSpace lexer

comma :: Monad m => Parser m String
comma = Token.comma lexer

commaSep :: Monad m => Parser m a -> Parser m [a]
commaSep = Token.commaSep lexer

semi :: Monad m => Parser m String
semi = Token.semi lexer

braces :: Monad m => Parser m a -> Parser m a
braces = Token.braces lexer

brackets :: Monad m => Parser m a -> Parser m a
brackets = Token.brackets lexer

locate :: Monad m => Parser m a -> Sirius m a
locate p = do
  start <- P.getPosition
  r <- p
  end <- P.getPosition
  return (r :>: (start, end))

stringLiteral :: Monad m => Parser m String
stringLiteral = Token.stringLiteral lexer

decimal :: Monad m => Parser m Integer
decimal = Token.decimal lexer

lexeme :: Monad m => Parser m a -> Parser m (Located a)
lexeme p = do
  p1 <- P.getPosition
  x <- p
  p2 <- P.getPosition
  whiteSpace
  return $ x :>: (p1, p2)

lexeme' :: Monad m => Parser m a -> Parser m a
lexeme' p = do
  x <- p
  whiteSpace
  return x

parseEither ::
     Sirius m a -> Sirius m b -> Parser m (Either (Located a) (Located b))
parseEither pa pb = (Left <$> P.try pa) <|> (Right <$> pb)

lowered :: Monad m => Parser m String
lowered =
  lexeme' $ do
    c <- P.lower
    cs <- many P.alphaNum
    return (c : cs)

capitalized :: Monad m => Parser m String
capitalized =
  lexeme' $ do
    c <- P.upper
    cs <- many P.alphaNum
    return (c : cs)

characterChar :: Monad m => Parser m Char
characterChar = charLetter <|> charEscape

charEscape :: Monad m => Parser m Char
charEscape = do
  _ <- P.char '\\'
  escapeCode

charLetter :: Monad m => Parser m Char
charLetter =
  P.satisfy (\c -> (c /= '\"') && (c /= '{') && (c /= '\\') && (c > '\026'))

escapeCode :: Monad m => Parser m Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <|> charLetter

charControl :: Monad m => Parser m Char
charControl = do
  _ <- P.char '^'
  code <- P.upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Monad m => Parser m Char
charNum = do
  code <-
    decimal <|> do
      _ <- P.char 'o'
      number 8 P.octDigit <|> do
        _ <- P.char 'x'
        number 16 P.hexDigit
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

number :: Monad m => Integer -> Parser m Char -> Parser m Integer
number base baseDigit = do
  digits <- P.many1 baseDigit
  let n = foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

charEsc :: Monad m => Parser m Char
charEsc = P.choice (map parseEsc escMap)
  where
    parseEsc (c, code) = do
      _ <- P.char c
      return code

charAscii :: Monad m => Parser m Char
charAscii = P.choice (map parseAscii asciiMap)
  where
    parseAscii (asc, code) =
      P.try
        (do _ <- P.string asc
            return code)

escMap :: [(Char, Char)]
escMap = zip "{abf{rtv\\\"\'" "{\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes =
  [ "BS"
  , "HT"
  , "LF"
  , "VT"
  , "FF"
  , "CR"
  , "SO"
  , "SI"
  , "EM"
  , "FS"
  , "GS"
  , "RS"
  , "US"
  , "SP"
  ]

ascii3codes :: [String]
ascii3codes =
  [ "NUL"
  , "SOH"
  , "STX"
  , "ETX"
  , "EOT"
  , "ENQ"
  , "ACK"
  , "BEL"
  , "DLE"
  , "DC1"
  , "DC2"
  , "DC3"
  , "DC4"
  , "NAK"
  , "SYN"
  , "ETB"
  , "CAN"
  , "SUB"
  , "ESC"
  , "DEL"
  ]

ascii2 :: String
ascii2 =
  [ '\BS'
  , '\HT'
  , '\LF'
  , '\VT'
  , '\FF'
  , '\CR'
  , '\SO'
  , '\SI'
  , '\EM'
  , '\FS'
  , '\GS'
  , '\RS'
  , '\US'
  , '\SP'
  ]

ascii3 :: String
ascii3 =
  [ '\NUL'
  , '\SOH'
  , '\STX'
  , '\ETX'
  , '\EOT'
  , '\ENQ'
  , '\ACK'
  , '\BEL'
  , '\DLE'
  , '\DC1'
  , '\DC2'
  , '\DC3'
  , '\DC4'
  , '\NAK'
  , '\SYN'
  , '\ETB'
  , '\CAN'
  , '\SUB'
  , '\ESC'
  , '\DEL'
  ]
