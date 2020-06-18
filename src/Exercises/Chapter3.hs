module Exercises.Chapter3 where

import Exercises.AST
import Exercises.Lexer
import Exercises.Parser

-- alex ./src/Exercises/Lexer.x -o ./src/Exercises/Lexer.hs
-- happy ./src/Exercises/Parser.y -o ./src/Exercises/Parser.hs

testlex = alexScanTokens "let a := 3 in a end"

parseFile :: IO ()
parseFile = do
  src <- readFile "./testcases/mytest.tig"
  let tokens = parseProgram src
  print tokens

--print $ parse $ fmap token tokens

-- >>> parseFile
-- Right (Let [FunDecl [Function {funName = Symbol {sym = "fact"}, funParams = [Field {fieldName = Symbol {sym = "a"}, fieldTy = Symbol {sym = "int"}, fieldAnnot = ()},Field {fieldName = Symbol {sym = "b"}, fieldTy = Symbol {sym = "int"}, fieldAnnot = ()}], funReturnTy = Just (Symbol {sym = "int"},()), funBody = Call (Symbol {sym = "fact"}) [Op (Var (SimpleVar (Symbol {sym = "a"}) ()) ()) Times (Var (SimpleVar (Symbol {sym = "b"}) ()) ()) (),Op (Var (SimpleVar (Symbol {sym = "b"}) ()) ()) Minus (Num 1 ()) ()] (), funAnnot = ()}] ()] (Seq [Call (Symbol {sym = "fact"}) [Num 1 (),Num 5 ()] ()] ()) ())
--
