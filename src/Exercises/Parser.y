{
{-# LANGUAGE OverloadedStrings #-}
module Exercises.Parser (parseProgram) where
import Control.Monad
import Data.Semigroup
import Data.String
import qualified Exercises.Lexer as L 
import Exercises.Lexer(Tok(..))
import Exercises.AST
}

%name program
%monad { P } { (>>=) } { return }
%error { parseError }
%tokentype { L.Tok }


%token
  INT         { L.Tok _  (L.LitInt _)  }
  ID          { L.Tok _  (L.Ident _ )  }
  STRING      { L.Tok _  (L.LitString _ )  }

  'var'       { L.Tok $$  L.Var       }
  'while'     { L.Tok $$  L.While     }
  'for'       { L.Tok $$  L.For       }
  'to'        { L.Tok $$  L.To        }
  'break'     { L.Tok $$  L.Break     }
  'let'       { L.Tok $$  L.Let       }
  'in'        { L.Tok $$  L.In        }
  'end'       { L.Tok $$  L.End       }
  'function'  { L.Tok $$  L.Function  }
  'type'      { L.Tok $$  L.Type      }
  'array'     { L.Tok $$  L.Array     }
  'if'        { L.Tok $$  L.If        }
  'then'      { L.Tok $$  L.Then      }
  'else'      { L.Tok $$  L.Else      }
  'do'        { L.Tok $$  L.Do        }
  'of'        { L.Tok $$  L.Of        }
  'nil'       { L.Tok $$  L.Nil       }
  ','         { L.Tok $$  L.Comma     }
  ':'         { L.Tok $$  L.Colon     }
  ';'         { L.Tok $$  L.Semicolon      }
  '('         { L.Tok $$  L.LParen      }
  ')'         { L.Tok $$  L.RParen      }
  '['         { L.Tok $$  L.LBracket    }
  ']'         { L.Tok $$  L.RBracket    }
  '{'         { L.Tok $$  L.LBrace    }
  '}'         { L.Tok $$  L.RBrace    }
  '.'         { L.Tok $$  L.Dot       }
  '+'         { L.Tok $$  L.TPlus      }
  '-'         { L.Tok $$  L.TMinus     }
  '*'         { L.Tok $$  L.TMul     }
  '/'         { L.Tok $$  L.TDiv    }
  '='         { L.Tok $$  L.Equal        }
  '<>'        { L.Tok $$  L.NotEqual       }
  '>'         { L.Tok $$  L.GreaterThan        }
  '<'         { L.Tok $$  L.LessThan        }
  '>='        { L.Tok $$  L.GreaterThanEqual        }
  '<='        { L.Tok $$  L.LessThanEqual        }
  '&'         { L.Tok $$  L.And       }
  '|'         { L.Tok $$  L.Or        }
  ':='        { L.Tok $$  L.Assign    }

-- precedence: low to high
%right    'of'
%nonassoc 'do'
%nonassoc 'else'
%nonassoc ':='
%left     '|'
%left     '&'
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%right    'of'
%nonassoc 'do'
%nonassoc 'else'
%nonassoc ':='
%left     '|'
%left     '&'
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     UMINUS

%%

program :: { Expr L }
  : exp { $1 }

exp :: { Expr L }
   :    
      lvalue      { Var $1 (sp $1) }
   | cmpexp      { $1 }
   | mathexp     { $1 }
   | app         { $1 }
   | array {$1}
   | record {$1}
   | boolexp     { $1 }
   | assign      { $1 }
   | control     {$1}
   | '(' exp ')' { $2 }
   | INT         { let L.Tok l (L.LitInt i) = $1 in Num i () }
   | STRING      { let L.Tok l (L.LitString s) = $1 in Str s () }
   | 'nil'       { Nil (sp $1) }


app :: { Expr L }
  : ID '(' args ')' { Call (newSym $1) $3 ((sp $1)<> (sp $4)) }

args :: { [Expr L] }
  : {- nil -}    { [] }
  | exp moreargs { $1 : $2 }

moreargs :: { [Expr L] }
  : {- nil -}          { [] }
  | ',' exp moreargs   { $2 : $3 }


decs :: { [Decl L] }
  : {- nil -}  { [] }
  | dec decs   { $1 : $2 }

dec :: { Decl L }
  : vardec  { $1 }
  | fundecs  { FunDecl $1 (foldMap sp $1) }
  | tydecs   { TypeDecl     $1 (foldMap (\ (_,_,x) -> x) $1) }

vardec :: { Decl L }
  : 'var' ID ':=' exp
     { VarDecl (newSym $2) Nothing $4 (spr $1 $4) }
  | 'var' ID ':' ID ':=' exp
     { VarDecl (newSym $2) (Just (newSym $4, sp $4)) $6 (spr $1 $6) }



fundecs :: { [Function L] }
  : fundec fundecs1 { $1 : $2 }

fundecs1 :: { [Function L] }
  : {- nil -} { [] }
  | fundec fundecs1 { $1 : $2 }

fundec :: { Function L }
  : 'function' ID '(' tyfields ')' '=' exp
     { Function (newSym $2) $4 Nothing      $7 (spr $1 $7) }
  | 'function' ID '(' tyfields ')' ':' ID '=' exp
     { Function (newSym $2) $4 (Just (newSym $7, sp $7)) $9 (spr $1 $9) }

tyfields :: { [Field L] }
  : {- nil -} { [] }
  | tyfield tyfields1 { $1 : $2 }

tyfield :: { Field L }
  : ID ':' ID { Field (newSym $1) (newSym $3) (spr $1 $3) }

tyfields1 :: { [Field L] }
  : {- nil -} { [] }
  | ',' tyfield tyfields1 { $2 : $3 }




tydecs :: { [(Symbol, Ty L, L)] }
  : tydec tydecs1 { $1 : $2 }

tydecs1 :: { [(Symbol, Ty L, L)] }
  : {- nil -} { [] }
  | tydec tydecs1 { $1 : $2 }

tydec :: { (Symbol, Ty L, L) }
  : 'type' ID '=' ty { (newSym $2, $4, (spr $1 $4)) }

ty :: { Ty L }
  : ID                { NameTy (newSym $1) (sp $1) }
  | '{' tyfields '}'  { RecordTy $2 (spr $1 $3) }
  | 'array' 'of' ID   { ArrayTy (newSym $3) (spr $1 $3) }



sequence :: { Expr L }
  : '(' sequence1 ')' { Seq $2 (sp $1 <> sp $3) }

sequence1 :: { [Expr L] }
  : {- nil -}      { [] }
  | exp sequence2  { $1 : $2 }

sequence2 :: { [Expr L] }
  : {- nil -}         { [] }
  | ';' exp sequence2 { $2 : $3 }






control :: {Expr L}
    :
      'if' exp 'then' exp 'else' exp { If $2 $4 (Just $6) (spr $1 $6) }
    | 'if' exp 'then' exp                   { If $2 $4  Nothing      (spr $1 $4) }
    | 'while' exp 'do' exp                  { While $2 $4            (spr $1 $4) }
    | 'for' ID ':=' exp 'to' exp 'do' exp   { For (newSym $2) $4 $6 $8  (spr $1 $8) }
    | 'break'                               { Break                  (sp $1)     }
    | 'let' decs 'in' sequence1 'end'       { Let $2 (Seq $4 (foldMap sp $4)) (spr $1 $5) }

assign :: { Expr L }
  : lvalue ':=' exp { Assign $1 $3 (spr $1 $3) }

lvalue :: { Var L }
  : ID      { SimpleVar (newSym $1) (sp $1) }
  | lvalue1 { $1 }

lvalue1 :: { Var L }
  : ID '.' ID            { FieldVar (SimpleVar (newSym $1) (sp $1)) (newSym $3) (spr $1 $3) }
  | lvalue1 '.' ID       { FieldVar $1 (newSym $3) (spr $1 $3)  }
  | ID '[' exp ']'       { SubsVar (SimpleVar (newSym $1) (sp $1)) $3 (spr $1 $4) }
  | lvalue1 '[' exp ']'  { SubsVar $1 $3 (spr $1 $4) }


cmpexp :: { Expr L }
  : exp '='   exp   { Op $1 Eq      $3 (spr $1 $3)  }
  | exp '<>'  exp   { Op $1 Neq     $3 (spr $1 $3)  }
  | exp '>'   exp   { Op $1 Lt      $3 (spr $1 $3)  }
  | exp '<'   exp   { Op $1 Gt      $3 (spr $1 $3)  }
  | exp '>='  exp   { Op $1 Le      $3 (spr $1 $3)  }
  | exp '<='  exp   { Op $1 Ge      $3 (spr $1 $3)  }

mathexp :: { Expr L }
  :         '-'  exp %prec UMINUS { Op (Num (-1) mempty) Times $2 (spr $1 $2)  }
  | exp '+'  exp              { Op $1 Plus   $3  (spr $1 $3) }
  | exp '-'  exp              { Op $1 Minus  $3  (spr $1 $3) }
  | exp '*'  exp              { Op $1 Times  $3  (spr $1 $3) }
  | exp '/'  exp              { Op $1 Divide $3  (spr $1 $3) }

boolexp :: { Expr L }
  : exp '&' exp               { If $1 $3 (Just (Num 0 mempty)) (spr $1 $3) }
  | exp '|' exp               { If $1 (Num 1 mempty) (Just $3) (spr $1 $3) }




record :: { Expr L }
  : ID '{' fields0 '}' { Record  (newSym $1) $3 (spr $1 $4) }

fields0 :: { [(Symbol, Expr L, L)] }
  : {- nil -}     { [] }
  | field1 fields { $1 : $2 }

field1 :: { (Symbol, Expr L, L) }
  : ID '=' exp { (newSym $1, $3, (spr $1 $3)) }

fields :: { [(Symbol, Expr L, L)] }
  : {- nil -}         { [] }
  | ',' field1 fields { $2 : $3 }

array :: { Expr L }
  : ID '[' exp ']' 'of' exp  { Array (newSym $1) $3 $6  (spr $1 $6) }




{
type P = Either String
type L = ()
parseError :: [L.Tok] -> P a
parseError toks = Left $ "A parse error occurred\n\n " <> show toks

spr _ _ = ()


newSym (Tok _ (L.Ident s)) = Symbol s

sp _ = ()

parseProgram :: String -> Either String (Expr L)
parseProgram = program <=< L.mylexer

}
