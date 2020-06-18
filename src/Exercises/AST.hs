module Exercises.AST where

import GHC.Exts (IsString (..))

data SrcSpan = SrcSpan
  { srcSpanStartLine :: !Int,
    srcSpanStartCol :: !Int,
    srcSpanEndLine :: !Int,
    srcSpanEndCol :: !Int
  }
  deriving (Show, Eq, Ord)

instance Semigroup SrcSpan where
  x@(SrcSpan sl1 sc1 el1 ec1) <> y@(SrcSpan sl2 sc2 el2 ec2)
    | x == mempty = y
    | y == mempty = x
    | otherwise =
      SrcSpan (min sl1 sl2) (min sc1 sc2) (max el1 el2) (max ec1 ec2)

instance Monoid SrcSpan where
  mempty = SrcSpan 0 0 0 0
  mappend = (<>)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Show, Eq, Enum, Bounded)

newtype Symbol = Symbol {sym :: String} deriving (Show, Eq, Ord)

instance IsString Symbol where
  fromString = Symbol

data Var a
  = SimpleVar Symbol a
  | FieldVar (Var a) Symbol a
  | SubsVar (Var a) (Expr a) a
  deriving (Show, Eq)

data Expr a
  = If (Expr a) (Expr a) (Maybe (Expr a)) a
  | While (Expr a) (Expr a) a
  | For Symbol (Expr a) (Expr a) (Expr a) a
  | Op (Expr a) Op (Expr a) a
  | Assign (Var a) (Expr a) a
  | Break a
  | Let [Decl a] (Expr a) a
  | Num Integer a
  | Str String a
  | Var (Var a) a
  | Seq [Expr a] a
  | Call Symbol [Expr a] a
  | Nil a
  | -- | record type [field name - initial expressiio]
    Record Symbol [(Symbol, Expr a, a)] a
  | -- | array type - size - initial value
    Array Symbol (Expr a) (Expr a) a
  deriving (Show, Eq)

data Decl a
  = FunDecl [Function a] a
  | TypeDecl [(Symbol, Ty a, a)] a
  | VarDecl {varName :: Symbol, varTy :: Maybe (Symbol, a), init :: Expr a, meta :: a}
  deriving (Show, Eq)

data Field a = Field
  { fieldName :: Symbol,
    fieldTy :: Symbol,
    fieldAnnot :: a
  }
  deriving (Show, Eq)

data Ty a
  = NameTy Symbol a
  | RecordTy [Field a] a
  | ArrayTy Symbol a
  deriving (Show, Eq)

data Function a = Function
  { funName :: Symbol,
    funParams :: [Field a],
    funReturnTy :: Maybe (Symbol, a),
    funBody :: Expr a,
    funAnnot :: a
  }
  deriving (Show, Eq)

class Ann f where
  ann :: f a -> a

instance Ann Var where
  ann (SimpleVar _ a) = a
  ann (FieldVar _ _ a) = a
  ann (SubsVar _ _ a) = a

instance Ann Expr where
  ann (Var _ a) = a
  ann (Nil a) = a
  ann (Num _ a) = a
  ann (Str _ a) = a
  ann (Call _ _ a) = a
  ann (Op _ _ _ a) = a
  ann (Record _ _ a) = a
  ann (Seq _ a) = a
  ann (Assign _ _ a) = a
  ann (If _ _ _ a) = a
  ann (While _ _ a) = a
  ann (For _ _ _ _ a) = a
  ann (Break a) = a
  ann (Let _ _ a) = a
  ann (Array _ _ _ a) = a

instance Ann Decl where
  ann (FunDecl _ a) = a
  ann VarDecl {meta = a} = a
  ann (TypeDecl _ a) = a
instance Ann Ty where

  ann (NameTy _ a) = a
  ann (RecordTy _ a) = a
  ann (ArrayTy _ a) = a

instance Ann Field where
  ann = fieldAnnot

instance Ann Function where
  ann = funAnnot