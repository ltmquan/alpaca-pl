module AST 
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
    ) where

newtype Prog = Prog [ProcDef]
    deriving (Show, Eq)

data ProcDef = PDef Id LocalProc Proc
    deriving (Show, Eq)

data LocalProc = LProc Id [Id]
    deriving (Show, Eq)

data Proc 
    = ParProc [SeqProc]
    | SeqProc SeqProc
    deriving (Show, Eq)

newtype SeqProc = SProc [Stmt]
    deriving (Show, Eq)

data Stmt
    = WaitStmt WaitOp Expr
    | SendStmt Id Expr
    deriving (Show, Eq)

data WaitOp = WOp (Either Id String) (Either Id String)
    deriving (Show, Eq)

data Expr
    = CastExpr String Expr
    | IdExpr Id
    | LitExpr Lit
    deriving (Show, Eq)

newtype Id = Ident String
    deriving (Show, Eq)

data Lit
    = ListLit [Expr]
    | IntLit Int
    | CharLit Char
    | StrLit String
    | Nil
    deriving (Show, Eq)
