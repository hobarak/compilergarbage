{
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches
    -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
module Exercises.Lexer  where
}

%wrapper "posn"
$digit = 0-9		 
$alpha = [a-zA-Z]		 
$u     = [ . \n ]
@ident = $alpha [$alpha $digit \_]* [\']*
tokens :-


  $white+         ;
  "/*" ($u)* "*/" ;
  while     { con While  }
  for       { con For  }
  to        { con To  }
  break     { con Break  }
  let       { con Let  }
  in        { con In  }
  end       { con End  }
  function  { con Function  }
  var       { con Var  }
  type      { con Type  }
  array     { con Array  }
  if        { con If  }
  then      { con Then  }
  else      { con Else  }
  do        { con Do  }
  of        { con Of  }
  nil       { con Nil  }
  ","       { con Comma  }
  ":"       { con Colon  }
  ";"       { con Semicolon  }
  "("       { con LParen  }
  ")"       { con RParen  }
  "["       { con LBracket  }
  "]"       { con RBracket  }
  "{"       { con LBrace  }
  "}"       { con RBrace  }
  "."       { con Dot  }
  "+"       { con TPlus  }
  "-"       { con TMinus  }
  "*"       { con TMul  }
  "/"       { con TDiv  }
  "="       { con Equal  }
  "<>"      { con NotEqual  }
  "<"       { con LessThan  }
  "<="      { con LessThanEqual  }
  ">"       { con GreaterThan  }
  ">="      { con GreaterThanEqual  }
  "&"       { con And  }
  "|"       { con Or  }
  ":="      { con Assign  }
  $digit+   {(\p s -> Tok p (LitInt (read s :: Integer))  ) }
  \"[^\"]*\"  { (\p (s:ss) -> Tok p (LitString (init ss)) ) }
  @ident      {(\p s -> Tok p (Ident s)  ) }


{

data Tok = Tok {pos :: AlexPosn, token :: Token} deriving (Eq,Show)


con g = \p s -> Tok  p g


data Token
  = While  
  | For  
  | To  
  | Break  
  | Let  
  | In  
  | End  
  | Function  
  | Var  
  | Type  
  | Array  
  | If  
  | Then  
  | Else  
  | Do  
  | Of  
  | Nil  
  | Comma  
  | Colon  
  | Semicolon  
  | LParen  
  | RParen  
  | LBracket  
  | RBracket  
  | LBrace  
  | RBrace  
  | Dot  
  | TPlus  
  | TMinus  
  | TMul  
  | TDiv  
  | Equal  
  | NotEqual  
  | LessThan  
  | LessThanEqual  
  | GreaterThan  
  | GreaterThanEqual  
  | And  
  | Or  
  | Assign  
  | Ident String
  | LitInt Integer
  | LitString String
  deriving (Eq, Show)


mylexer s  = return $ (alexScanTokens s)
}