{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseProg
    , parseProgFromFile
    , Parser
    , ParseError
    ) where

import AST
    ( Prog(..)
    , ProcDef(..)
    , LocalProc(..)
    , Proc(..)
    , SeqProc(..)
    , Stmt(..)
    , WaitOp(..)
    , Expr(..)
    , Id(..)
    , Lit(..)
    )
import Control.Applicative ((<|>), many, optional)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
    ( Parsec
    , ParseErrorBundle
    , parse
    , choice
    , try
    , eof
    , notFollowedBy
    , satisfy
    , sepBy
    , between
    )
import Text.Megaparsec.Char
    ( letterChar
    , alphaNumChar
    , char
    , string
    , space1
    )
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type alias
type Parser = Parsec Void Text

-- | Parse error type alias
type ParseError = ParseErrorBundle Text Void

-- ============================================================================
-- Lexer
-- ============================================================================

-- | Space consumer that handles whitespace and comments
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Lexeme combinator: parses a parser and consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser: parses a string and consumes trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a specific keyword
keyword :: Text -> Parser ()
keyword w = lexeme (string w *> notFollowedBy alphaNumChar)

-- | Parses content within parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses content within square brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parses content within curly braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Reserved keywords that cannot be used as identifiers
reserved :: [Text]
reserved = ["int", "str", "id", "nil"]

-- ============================================================================
-- Identifiers and Literals
-- ============================================================================

-- | Parse an identifier without consuming trailing whitespace
identifierNoSpace :: Parser Id
identifierNoSpace = do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let ident = T.pack (first : rest)
    if ident `elem` reserved
        then fail $ "keyword " ++ show ident ++ " cannot be an identifier"
        else return $ Ident (T.unpack ident)

-- | Parse an identifier
identifier :: Parser Id
identifier = lexeme identifierNoSpace

-- | Parse an integer literal
intLiteral :: Parser Lit
intLiteral = IntLit <$> lexeme L.decimal

-- | Parse a character literal
charLiteral :: Parser Lit
charLiteral = lexeme $ do
    _ <- char '\''
    c <- charChar
    _ <- char '\''
    return $ CharLit c

-- | Parse a string literal
stringLiteral :: Parser Lit
stringLiteral = lexeme $ do
    _ <- char '"'
    str <- many charChar
    _ <- char '"'
    return $ StrLit str

-- | Parse a character within a string or char literal
charChar :: Parser Char
charChar = escapeChar <|> satisfy (\c -> c /= '"' && c /= '\\' && c /= '\'')

-- | Parse an escape sequence
escapeChar :: Parser Char
escapeChar = char '\\' *> choice
    [ char 'n' $> '\n'
    , char 't' $> '\t'
    , char 'r' $> '\r'
    , char '\\' $> '\\'
    , char '"' $> '"'
    , char '\'' $> '\''
    ]

-- | Parse a list literal
listLiteral :: Parser Lit
listLiteral = ListLit <$> brackets (expr `sepBy` symbol ",")

-- | Parse nil
nilLiteral :: Parser Lit
nilLiteral = keyword "nil" $> Nil

-- | Parse any literal
literal :: Parser Lit
literal = choice
    [ try listLiteral
    , try intLiteral
    , try charLiteral
    , try stringLiteral
    , nilLiteral
    ]

-- ============================================================================
-- Expressions
-- ============================================================================

-- | Parse a cast operator
castOp :: Parser String
castOp = choice
    [ keyword "int" $> "int"
    , keyword "str" $> "str"
    , keyword "id" $> "id"
    ]

-- | Parse an expression
expr :: Parser Expr
expr = choice
    [ try castExpr
    , try parenExpr
    , try (IdExpr <$> identifier)
    , LitExpr <$> literal
    ]

-- | Parse a cast expression
castExpr :: Parser Expr
castExpr = do 
    op <- castOp
    CastExpr op <$> expr

-- | Parse a parenthesized expression
parenExpr :: Parser Expr
parenExpr = parens expr

-- ============================================================================
-- Statements and Processes
-- ============================================================================

-- | Parse a wait operation
-- Note: No whitespace allowed between $, first identifier, :, and second identifier
waitOp :: Parser WaitOp
waitOp = lexeme $ do
    _ <- char '$'
    first <- optional identifierNoSpace
    second <- optional (char ':' *> identifierNoSpace)

    let firstPart = maybe (Right "") Left first
    let secondPart = maybe (Right "") Left second

    return $ WOp firstPart secondPart

-- | Parse a statement
stmt :: Parser Stmt
stmt = do
    prefix <- choice
        [ Left <$> try waitOp
        , Right <$> identifier
        ]
    e <- expr
    _ <- symbol "."
    case prefix of
        Left wo -> return $ WaitStmt wo e
        Right _id -> return $ SendStmt _id e

-- | Parse a sequential process
seqProc :: Parser SeqProc
seqProc = SProc <$> braces (many stmt)

-- | Parse a parallel process
parProc :: Parser Proc
parProc = do
    _ <- symbol "|"
    first <- seqProc
    rest <- many (symbol "|" *> seqProc)
    return $ ParProc (first : rest)

-- | Parse a process (parallel or sequential)
proc :: Parser Proc
proc = choice
    [ try parProc
    , SeqProc <$> seqProc
    ]

-- | Parse local process parameters
localProc :: Parser LocalProc
localProc = do
    first <- brackets $ do
        id1 <- identifier
        rest <- many (symbol "," *> identifier)
        return (id1, rest)
    return $ uncurry LProc first

-- | Parse a process definition
procDef :: Parser ProcDef
procDef = do 
    name <- identifier
    params <- localProc
    _ <- symbol "="
    PDef name params <$> proc

-- ============================================================================
-- Top-level Program
-- ============================================================================

-- | Parse a complete program
program :: Parser Prog
program = do
    sc  -- Consume leading whitespace
    defs <- many procDef
    eof
    return $ Prog defs

-- ============================================================================
-- Public API
-- ============================================================================

-- | Parse a program from text
parseProg :: Text -> Either ParseError Prog
parseProg = parse program "<input>"

-- | Parse a program from a file
parseProgFromFile :: FilePath -> IO (Either ParseError Prog)
parseProgFromFile path = do
    content <- TIO.readFile path
    return $ parse program path content
