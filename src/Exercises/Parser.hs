{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module Exercises.Parser (parseProgram) where
import Control.Monad
import Data.Semigroup
import Data.String
import qualified Exercises.Lexer as L 
import Exercises.Lexer(Tok(..))
import Exercises.AST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (L.Tok)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Expr L)
	| HappyAbsSyn7 ([Expr L])
	| HappyAbsSyn9 ([Decl L])
	| HappyAbsSyn10 (Decl L)
	| HappyAbsSyn12 ([Function L])
	| HappyAbsSyn14 (Function L)
	| HappyAbsSyn15 ([Field L])
	| HappyAbsSyn16 (Field L)
	| HappyAbsSyn18 ([(Symbol, Ty L, L)])
	| HappyAbsSyn20 ((Symbol, Ty L, L))
	| HappyAbsSyn21 (Ty L)
	| HappyAbsSyn27 (Var L)
	| HappyAbsSyn33 ([(Symbol, Expr L, L)])
	| HappyAbsSyn34 ((Symbol, Expr L, L))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (L.Tok)
	-> HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> m HappyAbsSyn)
	-> [HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(L.Tok)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (L.Tok)
	-> HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> (P) HappyAbsSyn)
	-> [HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(L.Tok)] -> (P) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (L.Tok)
	-> HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> (P) HappyAbsSyn)
	-> [HappyState (L.Tok) (HappyStk HappyAbsSyn -> [(L.Tok)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(L.Tok)] -> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,793) ([0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,16380,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,8192,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,43008,2,0,0,0,0,0,0,0,7024,2180,8,0,0,32,0,0,0,0,0,0,0,0,0,32896,1,0,0,0,7024,2180,8,0,0,0,0,0,0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,16380,0,0,0,8,16380,0,0,8192,0,0,0,0,32896,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,32,0,0,0,0,32,0,0,0,0,0,0,16384,0,0,0,32,16380,0,0,7024,2180,8,0,0,7024,2180,8,0,0,32,0,0,0,0,32,0,0,0,0,7024,2180,8,0,0,32,0,0,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,8188,0,0,0,0,4092,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,48,0,0,0,0,16380,0,0,0,0,0,0,0,0,16384,16380,0,0,0,0,0,0,0,0,0,1,0,0,0,256,0,0,0,0,0,64,0,0,0,16384,16380,0,0,0,256,16380,0,0,0,4096,0,0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,64,0,0,0,2048,0,0,0,0,512,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,16380,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,7024,2180,8,0,0,32,0,0,0,0,32,32770,0,0,0,1024,0,16380,0,0,0,0,16380,0,0,0,0,0,0,0,0,0,0,0,0,7024,2180,8,0,0,0,64,0,0,0,7024,2180,8,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,16380,0,0,7024,2180,8,0,0,0,256,16380,0,0,7024,2180,8,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,32,0,0,0,0,0,4096,0,0,0,0,256,0,0,0,0,512,0,0,0,0,0,16380,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,7024,2180,8,0,0,7024,2180,8,0,0,0,0,16380,0,0,0,1024,16380,0,0,7024,2180,8,0,0,32,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,512,64,0,0,0,0,1,0,0,32,0,0,0,0,0,32,16380,0,0,0,0,0,0,0,0,0,16380,0,0,0,0,0,0,0,7024,2180,8,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,7024,2180,8,0,0,0,256,0,0,0,0,0,0,0,0,0,0,16380,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16380,0,0,0,0,64,0,0,0,0,16380,0,0,7024,2180,8,0,0,0,0,16380,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_program","program","exp","app","args","moreargs","decs","dec","vardec","fundecs","fundecs1","fundec","tyfields","tyfield","tyfields1","tydecs","tydecs1","tydec","ty","sequence","sequence1","sequence2","control","assign","lvalue","lvalue1","cmpexp","mathexp","boolexp","record","fields0","field1","fields","array","INT","ID","STRING","'var'","'while'","'for'","'to'","'break'","'let'","'in'","'end'","'function'","'type'","'array'","'if'","'then'","'else'","'do'","'of'","'nil'","','","':'","';'","'('","')'","'['","']'","'{'","'}'","'.'","'+'","'-'","'*'","'/'","'='","'<>'","'>'","'<'","'>='","'<='","'&'","'|'","':='","%eof"]
        bit_start = st * 80
        bit_end = (st + 1) * 80
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..79]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (37) = happyShift action_13
action_0 (38) = happyShift action_14
action_0 (39) = happyShift action_15
action_0 (41) = happyShift action_16
action_0 (42) = happyShift action_17
action_0 (44) = happyShift action_18
action_0 (45) = happyShift action_19
action_0 (51) = happyShift action_20
action_0 (56) = happyShift action_21
action_0 (60) = happyShift action_22
action_0 (68) = happyShift action_23
action_0 (4) = happyGoto action_24
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (25) = happyGoto action_4
action_0 (26) = happyGoto action_5
action_0 (27) = happyGoto action_6
action_0 (28) = happyGoto action_7
action_0 (29) = happyGoto action_8
action_0 (30) = happyGoto action_9
action_0 (31) = happyGoto action_10
action_0 (32) = happyGoto action_11
action_0 (36) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (37) = happyShift action_13
action_1 (38) = happyShift action_14
action_1 (39) = happyShift action_15
action_1 (41) = happyShift action_16
action_1 (42) = happyShift action_17
action_1 (44) = happyShift action_18
action_1 (45) = happyShift action_19
action_1 (51) = happyShift action_20
action_1 (56) = happyShift action_21
action_1 (60) = happyShift action_22
action_1 (68) = happyShift action_23
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (25) = happyGoto action_4
action_1 (26) = happyGoto action_5
action_1 (27) = happyGoto action_6
action_1 (28) = happyGoto action_7
action_1 (29) = happyGoto action_8
action_1 (30) = happyGoto action_9
action_1 (31) = happyGoto action_10
action_1 (32) = happyGoto action_11
action_1 (36) = happyGoto action_12
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (67) = happyShift action_47
action_2 (68) = happyShift action_48
action_2 (69) = happyShift action_49
action_2 (70) = happyShift action_50
action_2 (71) = happyShift action_51
action_2 (72) = happyShift action_52
action_2 (73) = happyShift action_53
action_2 (74) = happyShift action_54
action_2 (75) = happyShift action_55
action_2 (76) = happyShift action_56
action_2 (77) = happyShift action_57
action_2 (78) = happyShift action_58
action_2 _ = happyReduce_1

action_3 _ = happyReduce_5

action_4 _ = happyReduce_10

action_5 _ = happyReduce_9

action_6 (79) = happyShift action_46
action_6 _ = happyReduce_2

action_7 (62) = happyShift action_44
action_7 (66) = happyShift action_45
action_7 _ = happyReduce_57

action_8 _ = happyReduce_3

action_9 _ = happyReduce_4

action_10 _ = happyReduce_8

action_11 _ = happyReduce_7

action_12 _ = happyReduce_6

action_13 _ = happyReduce_12

action_14 (60) = happyShift action_40
action_14 (62) = happyShift action_41
action_14 (64) = happyShift action_42
action_14 (66) = happyShift action_43
action_14 _ = happyReduce_56

action_15 _ = happyReduce_13

action_16 (37) = happyShift action_13
action_16 (38) = happyShift action_14
action_16 (39) = happyShift action_15
action_16 (41) = happyShift action_16
action_16 (42) = happyShift action_17
action_16 (44) = happyShift action_18
action_16 (45) = happyShift action_19
action_16 (51) = happyShift action_20
action_16 (56) = happyShift action_21
action_16 (60) = happyShift action_22
action_16 (68) = happyShift action_23
action_16 (5) = happyGoto action_39
action_16 (6) = happyGoto action_3
action_16 (25) = happyGoto action_4
action_16 (26) = happyGoto action_5
action_16 (27) = happyGoto action_6
action_16 (28) = happyGoto action_7
action_16 (29) = happyGoto action_8
action_16 (30) = happyGoto action_9
action_16 (31) = happyGoto action_10
action_16 (32) = happyGoto action_11
action_16 (36) = happyGoto action_12
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (38) = happyShift action_38
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_53

action_19 (40) = happyShift action_35
action_19 (48) = happyShift action_36
action_19 (49) = happyShift action_37
action_19 (9) = happyGoto action_28
action_19 (10) = happyGoto action_29
action_19 (11) = happyGoto action_30
action_19 (12) = happyGoto action_31
action_19 (14) = happyGoto action_32
action_19 (18) = happyGoto action_33
action_19 (20) = happyGoto action_34
action_19 _ = happyReduce_20

action_20 (37) = happyShift action_13
action_20 (38) = happyShift action_14
action_20 (39) = happyShift action_15
action_20 (41) = happyShift action_16
action_20 (42) = happyShift action_17
action_20 (44) = happyShift action_18
action_20 (45) = happyShift action_19
action_20 (51) = happyShift action_20
action_20 (56) = happyShift action_21
action_20 (60) = happyShift action_22
action_20 (68) = happyShift action_23
action_20 (5) = happyGoto action_27
action_20 (6) = happyGoto action_3
action_20 (25) = happyGoto action_4
action_20 (26) = happyGoto action_5
action_20 (27) = happyGoto action_6
action_20 (28) = happyGoto action_7
action_20 (29) = happyGoto action_8
action_20 (30) = happyGoto action_9
action_20 (31) = happyGoto action_10
action_20 (32) = happyGoto action_11
action_20 (36) = happyGoto action_12
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_14

action_22 (37) = happyShift action_13
action_22 (38) = happyShift action_14
action_22 (39) = happyShift action_15
action_22 (41) = happyShift action_16
action_22 (42) = happyShift action_17
action_22 (44) = happyShift action_18
action_22 (45) = happyShift action_19
action_22 (51) = happyShift action_20
action_22 (56) = happyShift action_21
action_22 (60) = happyShift action_22
action_22 (68) = happyShift action_23
action_22 (5) = happyGoto action_26
action_22 (6) = happyGoto action_3
action_22 (25) = happyGoto action_4
action_22 (26) = happyGoto action_5
action_22 (27) = happyGoto action_6
action_22 (28) = happyGoto action_7
action_22 (29) = happyGoto action_8
action_22 (30) = happyGoto action_9
action_22 (31) = happyGoto action_10
action_22 (32) = happyGoto action_11
action_22 (36) = happyGoto action_12
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (37) = happyShift action_13
action_23 (38) = happyShift action_14
action_23 (39) = happyShift action_15
action_23 (41) = happyShift action_16
action_23 (42) = happyShift action_17
action_23 (44) = happyShift action_18
action_23 (45) = happyShift action_19
action_23 (51) = happyShift action_20
action_23 (56) = happyShift action_21
action_23 (60) = happyShift action_22
action_23 (68) = happyShift action_23
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_3
action_23 (25) = happyGoto action_4
action_23 (26) = happyGoto action_5
action_23 (27) = happyGoto action_6
action_23 (28) = happyGoto action_7
action_23 (29) = happyGoto action_8
action_23 (30) = happyGoto action_9
action_23 (31) = happyGoto action_10
action_23 (32) = happyGoto action_11
action_23 (36) = happyGoto action_12
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (80) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_68

action_26 (61) = happyShift action_93
action_26 (67) = happyShift action_47
action_26 (68) = happyShift action_48
action_26 (69) = happyShift action_49
action_26 (70) = happyShift action_50
action_26 (71) = happyShift action_51
action_26 (72) = happyShift action_52
action_26 (73) = happyShift action_53
action_26 (74) = happyShift action_54
action_26 (75) = happyShift action_55
action_26 (76) = happyShift action_56
action_26 (77) = happyShift action_57
action_26 (78) = happyShift action_58
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (52) = happyShift action_92
action_27 (67) = happyShift action_47
action_27 (68) = happyShift action_48
action_27 (69) = happyShift action_49
action_27 (70) = happyShift action_50
action_27 (71) = happyShift action_51
action_27 (72) = happyShift action_52
action_27 (73) = happyShift action_53
action_27 (74) = happyShift action_54
action_27 (75) = happyShift action_55
action_27 (76) = happyShift action_56
action_27 (77) = happyShift action_57
action_27 (78) = happyShift action_58
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (46) = happyShift action_91
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (40) = happyShift action_35
action_29 (48) = happyShift action_36
action_29 (49) = happyShift action_37
action_29 (9) = happyGoto action_90
action_29 (10) = happyGoto action_29
action_29 (11) = happyGoto action_30
action_29 (12) = happyGoto action_31
action_29 (14) = happyGoto action_32
action_29 (18) = happyGoto action_33
action_29 (20) = happyGoto action_34
action_29 _ = happyReduce_20

action_30 _ = happyReduce_22

action_31 _ = happyReduce_23

action_32 (48) = happyShift action_36
action_32 (13) = happyGoto action_88
action_32 (14) = happyGoto action_89
action_32 _ = happyReduce_28

action_33 _ = happyReduce_24

action_34 (49) = happyShift action_37
action_34 (19) = happyGoto action_86
action_34 (20) = happyGoto action_87
action_34 _ = happyReduce_38

action_35 (38) = happyShift action_85
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (38) = happyShift action_84
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (38) = happyShift action_83
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (79) = happyShift action_82
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (54) = happyShift action_81
action_39 (67) = happyShift action_47
action_39 (68) = happyShift action_48
action_39 (69) = happyShift action_49
action_39 (70) = happyShift action_50
action_39 (71) = happyShift action_51
action_39 (72) = happyShift action_52
action_39 (73) = happyShift action_53
action_39 (74) = happyShift action_54
action_39 (75) = happyShift action_55
action_39 (76) = happyShift action_56
action_39 (77) = happyShift action_57
action_39 (78) = happyShift action_58
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (37) = happyShift action_13
action_40 (38) = happyShift action_14
action_40 (39) = happyShift action_15
action_40 (41) = happyShift action_16
action_40 (42) = happyShift action_17
action_40 (44) = happyShift action_18
action_40 (45) = happyShift action_19
action_40 (51) = happyShift action_20
action_40 (56) = happyShift action_21
action_40 (60) = happyShift action_22
action_40 (68) = happyShift action_23
action_40 (5) = happyGoto action_79
action_40 (6) = happyGoto action_3
action_40 (7) = happyGoto action_80
action_40 (25) = happyGoto action_4
action_40 (26) = happyGoto action_5
action_40 (27) = happyGoto action_6
action_40 (28) = happyGoto action_7
action_40 (29) = happyGoto action_8
action_40 (30) = happyGoto action_9
action_40 (31) = happyGoto action_10
action_40 (32) = happyGoto action_11
action_40 (36) = happyGoto action_12
action_40 _ = happyReduce_16

action_41 (37) = happyShift action_13
action_41 (38) = happyShift action_14
action_41 (39) = happyShift action_15
action_41 (41) = happyShift action_16
action_41 (42) = happyShift action_17
action_41 (44) = happyShift action_18
action_41 (45) = happyShift action_19
action_41 (51) = happyShift action_20
action_41 (56) = happyShift action_21
action_41 (60) = happyShift action_22
action_41 (68) = happyShift action_23
action_41 (5) = happyGoto action_78
action_41 (6) = happyGoto action_3
action_41 (25) = happyGoto action_4
action_41 (26) = happyGoto action_5
action_41 (27) = happyGoto action_6
action_41 (28) = happyGoto action_7
action_41 (29) = happyGoto action_8
action_41 (30) = happyGoto action_9
action_41 (31) = happyGoto action_10
action_41 (32) = happyGoto action_11
action_41 (36) = happyGoto action_12
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (38) = happyShift action_77
action_42 (33) = happyGoto action_75
action_42 (34) = happyGoto action_76
action_42 _ = happyReduce_76

action_43 (38) = happyShift action_74
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (37) = happyShift action_13
action_44 (38) = happyShift action_14
action_44 (39) = happyShift action_15
action_44 (41) = happyShift action_16
action_44 (42) = happyShift action_17
action_44 (44) = happyShift action_18
action_44 (45) = happyShift action_19
action_44 (51) = happyShift action_20
action_44 (56) = happyShift action_21
action_44 (60) = happyShift action_22
action_44 (68) = happyShift action_23
action_44 (5) = happyGoto action_73
action_44 (6) = happyGoto action_3
action_44 (25) = happyGoto action_4
action_44 (26) = happyGoto action_5
action_44 (27) = happyGoto action_6
action_44 (28) = happyGoto action_7
action_44 (29) = happyGoto action_8
action_44 (30) = happyGoto action_9
action_44 (31) = happyGoto action_10
action_44 (32) = happyGoto action_11
action_44 (36) = happyGoto action_12
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (38) = happyShift action_72
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (37) = happyShift action_13
action_46 (38) = happyShift action_14
action_46 (39) = happyShift action_15
action_46 (41) = happyShift action_16
action_46 (42) = happyShift action_17
action_46 (44) = happyShift action_18
action_46 (45) = happyShift action_19
action_46 (51) = happyShift action_20
action_46 (56) = happyShift action_21
action_46 (60) = happyShift action_22
action_46 (68) = happyShift action_23
action_46 (5) = happyGoto action_71
action_46 (6) = happyGoto action_3
action_46 (25) = happyGoto action_4
action_46 (26) = happyGoto action_5
action_46 (27) = happyGoto action_6
action_46 (28) = happyGoto action_7
action_46 (29) = happyGoto action_8
action_46 (30) = happyGoto action_9
action_46 (31) = happyGoto action_10
action_46 (32) = happyGoto action_11
action_46 (36) = happyGoto action_12
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (37) = happyShift action_13
action_47 (38) = happyShift action_14
action_47 (39) = happyShift action_15
action_47 (41) = happyShift action_16
action_47 (42) = happyShift action_17
action_47 (44) = happyShift action_18
action_47 (45) = happyShift action_19
action_47 (51) = happyShift action_20
action_47 (56) = happyShift action_21
action_47 (60) = happyShift action_22
action_47 (68) = happyShift action_23
action_47 (5) = happyGoto action_70
action_47 (6) = happyGoto action_3
action_47 (25) = happyGoto action_4
action_47 (26) = happyGoto action_5
action_47 (27) = happyGoto action_6
action_47 (28) = happyGoto action_7
action_47 (29) = happyGoto action_8
action_47 (30) = happyGoto action_9
action_47 (31) = happyGoto action_10
action_47 (32) = happyGoto action_11
action_47 (36) = happyGoto action_12
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (37) = happyShift action_13
action_48 (38) = happyShift action_14
action_48 (39) = happyShift action_15
action_48 (41) = happyShift action_16
action_48 (42) = happyShift action_17
action_48 (44) = happyShift action_18
action_48 (45) = happyShift action_19
action_48 (51) = happyShift action_20
action_48 (56) = happyShift action_21
action_48 (60) = happyShift action_22
action_48 (68) = happyShift action_23
action_48 (5) = happyGoto action_69
action_48 (6) = happyGoto action_3
action_48 (25) = happyGoto action_4
action_48 (26) = happyGoto action_5
action_48 (27) = happyGoto action_6
action_48 (28) = happyGoto action_7
action_48 (29) = happyGoto action_8
action_48 (30) = happyGoto action_9
action_48 (31) = happyGoto action_10
action_48 (32) = happyGoto action_11
action_48 (36) = happyGoto action_12
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (37) = happyShift action_13
action_49 (38) = happyShift action_14
action_49 (39) = happyShift action_15
action_49 (41) = happyShift action_16
action_49 (42) = happyShift action_17
action_49 (44) = happyShift action_18
action_49 (45) = happyShift action_19
action_49 (51) = happyShift action_20
action_49 (56) = happyShift action_21
action_49 (60) = happyShift action_22
action_49 (68) = happyShift action_23
action_49 (5) = happyGoto action_68
action_49 (6) = happyGoto action_3
action_49 (25) = happyGoto action_4
action_49 (26) = happyGoto action_5
action_49 (27) = happyGoto action_6
action_49 (28) = happyGoto action_7
action_49 (29) = happyGoto action_8
action_49 (30) = happyGoto action_9
action_49 (31) = happyGoto action_10
action_49 (32) = happyGoto action_11
action_49 (36) = happyGoto action_12
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (37) = happyShift action_13
action_50 (38) = happyShift action_14
action_50 (39) = happyShift action_15
action_50 (41) = happyShift action_16
action_50 (42) = happyShift action_17
action_50 (44) = happyShift action_18
action_50 (45) = happyShift action_19
action_50 (51) = happyShift action_20
action_50 (56) = happyShift action_21
action_50 (60) = happyShift action_22
action_50 (68) = happyShift action_23
action_50 (5) = happyGoto action_67
action_50 (6) = happyGoto action_3
action_50 (25) = happyGoto action_4
action_50 (26) = happyGoto action_5
action_50 (27) = happyGoto action_6
action_50 (28) = happyGoto action_7
action_50 (29) = happyGoto action_8
action_50 (30) = happyGoto action_9
action_50 (31) = happyGoto action_10
action_50 (32) = happyGoto action_11
action_50 (36) = happyGoto action_12
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (37) = happyShift action_13
action_51 (38) = happyShift action_14
action_51 (39) = happyShift action_15
action_51 (41) = happyShift action_16
action_51 (42) = happyShift action_17
action_51 (44) = happyShift action_18
action_51 (45) = happyShift action_19
action_51 (51) = happyShift action_20
action_51 (56) = happyShift action_21
action_51 (60) = happyShift action_22
action_51 (68) = happyShift action_23
action_51 (5) = happyGoto action_66
action_51 (6) = happyGoto action_3
action_51 (25) = happyGoto action_4
action_51 (26) = happyGoto action_5
action_51 (27) = happyGoto action_6
action_51 (28) = happyGoto action_7
action_51 (29) = happyGoto action_8
action_51 (30) = happyGoto action_9
action_51 (31) = happyGoto action_10
action_51 (32) = happyGoto action_11
action_51 (36) = happyGoto action_12
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (37) = happyShift action_13
action_52 (38) = happyShift action_14
action_52 (39) = happyShift action_15
action_52 (41) = happyShift action_16
action_52 (42) = happyShift action_17
action_52 (44) = happyShift action_18
action_52 (45) = happyShift action_19
action_52 (51) = happyShift action_20
action_52 (56) = happyShift action_21
action_52 (60) = happyShift action_22
action_52 (68) = happyShift action_23
action_52 (5) = happyGoto action_65
action_52 (6) = happyGoto action_3
action_52 (25) = happyGoto action_4
action_52 (26) = happyGoto action_5
action_52 (27) = happyGoto action_6
action_52 (28) = happyGoto action_7
action_52 (29) = happyGoto action_8
action_52 (30) = happyGoto action_9
action_52 (31) = happyGoto action_10
action_52 (32) = happyGoto action_11
action_52 (36) = happyGoto action_12
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (37) = happyShift action_13
action_53 (38) = happyShift action_14
action_53 (39) = happyShift action_15
action_53 (41) = happyShift action_16
action_53 (42) = happyShift action_17
action_53 (44) = happyShift action_18
action_53 (45) = happyShift action_19
action_53 (51) = happyShift action_20
action_53 (56) = happyShift action_21
action_53 (60) = happyShift action_22
action_53 (68) = happyShift action_23
action_53 (5) = happyGoto action_64
action_53 (6) = happyGoto action_3
action_53 (25) = happyGoto action_4
action_53 (26) = happyGoto action_5
action_53 (27) = happyGoto action_6
action_53 (28) = happyGoto action_7
action_53 (29) = happyGoto action_8
action_53 (30) = happyGoto action_9
action_53 (31) = happyGoto action_10
action_53 (32) = happyGoto action_11
action_53 (36) = happyGoto action_12
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (37) = happyShift action_13
action_54 (38) = happyShift action_14
action_54 (39) = happyShift action_15
action_54 (41) = happyShift action_16
action_54 (42) = happyShift action_17
action_54 (44) = happyShift action_18
action_54 (45) = happyShift action_19
action_54 (51) = happyShift action_20
action_54 (56) = happyShift action_21
action_54 (60) = happyShift action_22
action_54 (68) = happyShift action_23
action_54 (5) = happyGoto action_63
action_54 (6) = happyGoto action_3
action_54 (25) = happyGoto action_4
action_54 (26) = happyGoto action_5
action_54 (27) = happyGoto action_6
action_54 (28) = happyGoto action_7
action_54 (29) = happyGoto action_8
action_54 (30) = happyGoto action_9
action_54 (31) = happyGoto action_10
action_54 (32) = happyGoto action_11
action_54 (36) = happyGoto action_12
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (37) = happyShift action_13
action_55 (38) = happyShift action_14
action_55 (39) = happyShift action_15
action_55 (41) = happyShift action_16
action_55 (42) = happyShift action_17
action_55 (44) = happyShift action_18
action_55 (45) = happyShift action_19
action_55 (51) = happyShift action_20
action_55 (56) = happyShift action_21
action_55 (60) = happyShift action_22
action_55 (68) = happyShift action_23
action_55 (5) = happyGoto action_62
action_55 (6) = happyGoto action_3
action_55 (25) = happyGoto action_4
action_55 (26) = happyGoto action_5
action_55 (27) = happyGoto action_6
action_55 (28) = happyGoto action_7
action_55 (29) = happyGoto action_8
action_55 (30) = happyGoto action_9
action_55 (31) = happyGoto action_10
action_55 (32) = happyGoto action_11
action_55 (36) = happyGoto action_12
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (37) = happyShift action_13
action_56 (38) = happyShift action_14
action_56 (39) = happyShift action_15
action_56 (41) = happyShift action_16
action_56 (42) = happyShift action_17
action_56 (44) = happyShift action_18
action_56 (45) = happyShift action_19
action_56 (51) = happyShift action_20
action_56 (56) = happyShift action_21
action_56 (60) = happyShift action_22
action_56 (68) = happyShift action_23
action_56 (5) = happyGoto action_61
action_56 (6) = happyGoto action_3
action_56 (25) = happyGoto action_4
action_56 (26) = happyGoto action_5
action_56 (27) = happyGoto action_6
action_56 (28) = happyGoto action_7
action_56 (29) = happyGoto action_8
action_56 (30) = happyGoto action_9
action_56 (31) = happyGoto action_10
action_56 (32) = happyGoto action_11
action_56 (36) = happyGoto action_12
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (37) = happyShift action_13
action_57 (38) = happyShift action_14
action_57 (39) = happyShift action_15
action_57 (41) = happyShift action_16
action_57 (42) = happyShift action_17
action_57 (44) = happyShift action_18
action_57 (45) = happyShift action_19
action_57 (51) = happyShift action_20
action_57 (56) = happyShift action_21
action_57 (60) = happyShift action_22
action_57 (68) = happyShift action_23
action_57 (5) = happyGoto action_60
action_57 (6) = happyGoto action_3
action_57 (25) = happyGoto action_4
action_57 (26) = happyGoto action_5
action_57 (27) = happyGoto action_6
action_57 (28) = happyGoto action_7
action_57 (29) = happyGoto action_8
action_57 (30) = happyGoto action_9
action_57 (31) = happyGoto action_10
action_57 (32) = happyGoto action_11
action_57 (36) = happyGoto action_12
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (37) = happyShift action_13
action_58 (38) = happyShift action_14
action_58 (39) = happyShift action_15
action_58 (41) = happyShift action_16
action_58 (42) = happyShift action_17
action_58 (44) = happyShift action_18
action_58 (45) = happyShift action_19
action_58 (51) = happyShift action_20
action_58 (56) = happyShift action_21
action_58 (60) = happyShift action_22
action_58 (68) = happyShift action_23
action_58 (5) = happyGoto action_59
action_58 (6) = happyGoto action_3
action_58 (25) = happyGoto action_4
action_58 (26) = happyGoto action_5
action_58 (27) = happyGoto action_6
action_58 (28) = happyGoto action_7
action_58 (29) = happyGoto action_8
action_58 (30) = happyGoto action_9
action_58 (31) = happyGoto action_10
action_58 (32) = happyGoto action_11
action_58 (36) = happyGoto action_12
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (67) = happyShift action_47
action_59 (68) = happyShift action_48
action_59 (69) = happyShift action_49
action_59 (70) = happyShift action_50
action_59 (71) = happyShift action_51
action_59 (72) = happyShift action_52
action_59 (73) = happyShift action_53
action_59 (74) = happyShift action_54
action_59 (75) = happyShift action_55
action_59 (76) = happyShift action_56
action_59 (77) = happyShift action_57
action_59 _ = happyReduce_74

action_60 (67) = happyShift action_47
action_60 (68) = happyShift action_48
action_60 (69) = happyShift action_49
action_60 (70) = happyShift action_50
action_60 (71) = happyShift action_51
action_60 (72) = happyShift action_52
action_60 (73) = happyShift action_53
action_60 (74) = happyShift action_54
action_60 (75) = happyShift action_55
action_60 (76) = happyShift action_56
action_60 _ = happyReduce_73

action_61 (67) = happyShift action_47
action_61 (68) = happyShift action_48
action_61 (69) = happyShift action_49
action_61 (70) = happyShift action_50
action_61 (71) = happyFail []
action_61 (72) = happyFail []
action_61 (73) = happyFail []
action_61 (74) = happyFail []
action_61 (75) = happyFail []
action_61 (76) = happyFail []
action_61 _ = happyReduce_67

action_62 (67) = happyShift action_47
action_62 (68) = happyShift action_48
action_62 (69) = happyShift action_49
action_62 (70) = happyShift action_50
action_62 (71) = happyFail []
action_62 (72) = happyFail []
action_62 (73) = happyFail []
action_62 (74) = happyFail []
action_62 (75) = happyFail []
action_62 (76) = happyFail []
action_62 _ = happyReduce_66

action_63 (67) = happyShift action_47
action_63 (68) = happyShift action_48
action_63 (69) = happyShift action_49
action_63 (70) = happyShift action_50
action_63 (71) = happyFail []
action_63 (72) = happyFail []
action_63 (73) = happyFail []
action_63 (74) = happyFail []
action_63 (75) = happyFail []
action_63 (76) = happyFail []
action_63 _ = happyReduce_65

action_64 (67) = happyShift action_47
action_64 (68) = happyShift action_48
action_64 (69) = happyShift action_49
action_64 (70) = happyShift action_50
action_64 (71) = happyFail []
action_64 (72) = happyFail []
action_64 (73) = happyFail []
action_64 (74) = happyFail []
action_64 (75) = happyFail []
action_64 (76) = happyFail []
action_64 _ = happyReduce_64

action_65 (67) = happyShift action_47
action_65 (68) = happyShift action_48
action_65 (69) = happyShift action_49
action_65 (70) = happyShift action_50
action_65 (71) = happyFail []
action_65 (72) = happyFail []
action_65 (73) = happyFail []
action_65 (74) = happyFail []
action_65 (75) = happyFail []
action_65 (76) = happyFail []
action_65 _ = happyReduce_63

action_66 (67) = happyShift action_47
action_66 (68) = happyShift action_48
action_66 (69) = happyShift action_49
action_66 (70) = happyShift action_50
action_66 (71) = happyFail []
action_66 (72) = happyFail []
action_66 (73) = happyFail []
action_66 (74) = happyFail []
action_66 (75) = happyFail []
action_66 (76) = happyFail []
action_66 _ = happyReduce_62

action_67 _ = happyReduce_72

action_68 _ = happyReduce_71

action_69 (69) = happyShift action_49
action_69 (70) = happyShift action_50
action_69 _ = happyReduce_70

action_70 (69) = happyShift action_49
action_70 (70) = happyShift action_50
action_70 _ = happyReduce_69

action_71 (67) = happyShift action_47
action_71 (68) = happyShift action_48
action_71 (69) = happyShift action_49
action_71 (70) = happyShift action_50
action_71 (71) = happyShift action_51
action_71 (72) = happyShift action_52
action_71 (73) = happyShift action_53
action_71 (74) = happyShift action_54
action_71 (75) = happyShift action_55
action_71 (76) = happyShift action_56
action_71 (77) = happyShift action_57
action_71 (78) = happyShift action_58
action_71 _ = happyReduce_55

action_72 _ = happyReduce_59

action_73 (63) = happyShift action_113
action_73 (67) = happyShift action_47
action_73 (68) = happyShift action_48
action_73 (69) = happyShift action_49
action_73 (70) = happyShift action_50
action_73 (71) = happyShift action_51
action_73 (72) = happyShift action_52
action_73 (73) = happyShift action_53
action_73 (74) = happyShift action_54
action_73 (75) = happyShift action_55
action_73 (76) = happyShift action_56
action_73 (77) = happyShift action_57
action_73 (78) = happyShift action_58
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_58

action_75 (65) = happyShift action_112
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (57) = happyShift action_111
action_76 (35) = happyGoto action_110
action_76 _ = happyReduce_79

action_77 (71) = happyShift action_109
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (63) = happyShift action_108
action_78 (67) = happyShift action_47
action_78 (68) = happyShift action_48
action_78 (69) = happyShift action_49
action_78 (70) = happyShift action_50
action_78 (71) = happyShift action_51
action_78 (72) = happyShift action_52
action_78 (73) = happyShift action_53
action_78 (74) = happyShift action_54
action_78 (75) = happyShift action_55
action_78 (76) = happyShift action_56
action_78 (77) = happyShift action_57
action_78 (78) = happyShift action_58
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (57) = happyShift action_107
action_79 (67) = happyShift action_47
action_79 (68) = happyShift action_48
action_79 (69) = happyShift action_49
action_79 (70) = happyShift action_50
action_79 (71) = happyShift action_51
action_79 (72) = happyShift action_52
action_79 (73) = happyShift action_53
action_79 (74) = happyShift action_54
action_79 (75) = happyShift action_55
action_79 (76) = happyShift action_56
action_79 (77) = happyShift action_57
action_79 (78) = happyShift action_58
action_79 (8) = happyGoto action_106
action_79 _ = happyReduce_18

action_80 (61) = happyShift action_105
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (37) = happyShift action_13
action_81 (38) = happyShift action_14
action_81 (39) = happyShift action_15
action_81 (41) = happyShift action_16
action_81 (42) = happyShift action_17
action_81 (44) = happyShift action_18
action_81 (45) = happyShift action_19
action_81 (51) = happyShift action_20
action_81 (56) = happyShift action_21
action_81 (60) = happyShift action_22
action_81 (68) = happyShift action_23
action_81 (5) = happyGoto action_104
action_81 (6) = happyGoto action_3
action_81 (25) = happyGoto action_4
action_81 (26) = happyGoto action_5
action_81 (27) = happyGoto action_6
action_81 (28) = happyGoto action_7
action_81 (29) = happyGoto action_8
action_81 (30) = happyGoto action_9
action_81 (31) = happyGoto action_10
action_81 (32) = happyGoto action_11
action_81 (36) = happyGoto action_12
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (37) = happyShift action_13
action_82 (38) = happyShift action_14
action_82 (39) = happyShift action_15
action_82 (41) = happyShift action_16
action_82 (42) = happyShift action_17
action_82 (44) = happyShift action_18
action_82 (45) = happyShift action_19
action_82 (51) = happyShift action_20
action_82 (56) = happyShift action_21
action_82 (60) = happyShift action_22
action_82 (68) = happyShift action_23
action_82 (5) = happyGoto action_103
action_82 (6) = happyGoto action_3
action_82 (25) = happyGoto action_4
action_82 (26) = happyGoto action_5
action_82 (27) = happyGoto action_6
action_82 (28) = happyGoto action_7
action_82 (29) = happyGoto action_8
action_82 (30) = happyGoto action_9
action_82 (31) = happyGoto action_10
action_82 (32) = happyGoto action_11
action_82 (36) = happyGoto action_12
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (71) = happyShift action_102
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (60) = happyShift action_101
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (58) = happyShift action_99
action_85 (79) = happyShift action_100
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_37

action_87 (49) = happyShift action_37
action_87 (19) = happyGoto action_98
action_87 (20) = happyGoto action_87
action_87 _ = happyReduce_38

action_88 _ = happyReduce_27

action_89 (48) = happyShift action_36
action_89 (13) = happyGoto action_97
action_89 (14) = happyGoto action_89
action_89 _ = happyReduce_28

action_90 _ = happyReduce_21

action_91 (37) = happyShift action_13
action_91 (38) = happyShift action_14
action_91 (39) = happyShift action_15
action_91 (41) = happyShift action_16
action_91 (42) = happyShift action_17
action_91 (44) = happyShift action_18
action_91 (45) = happyShift action_19
action_91 (51) = happyShift action_20
action_91 (56) = happyShift action_21
action_91 (60) = happyShift action_22
action_91 (68) = happyShift action_23
action_91 (5) = happyGoto action_95
action_91 (6) = happyGoto action_3
action_91 (23) = happyGoto action_96
action_91 (25) = happyGoto action_4
action_91 (26) = happyGoto action_5
action_91 (27) = happyGoto action_6
action_91 (28) = happyGoto action_7
action_91 (29) = happyGoto action_8
action_91 (30) = happyGoto action_9
action_91 (31) = happyGoto action_10
action_91 (32) = happyGoto action_11
action_91 (36) = happyGoto action_12
action_91 _ = happyReduce_45

action_92 (37) = happyShift action_13
action_92 (38) = happyShift action_14
action_92 (39) = happyShift action_15
action_92 (41) = happyShift action_16
action_92 (42) = happyShift action_17
action_92 (44) = happyShift action_18
action_92 (45) = happyShift action_19
action_92 (51) = happyShift action_20
action_92 (56) = happyShift action_21
action_92 (60) = happyShift action_22
action_92 (68) = happyShift action_23
action_92 (5) = happyGoto action_94
action_92 (6) = happyGoto action_3
action_92 (25) = happyGoto action_4
action_92 (26) = happyGoto action_5
action_92 (27) = happyGoto action_6
action_92 (28) = happyGoto action_7
action_92 (29) = happyGoto action_8
action_92 (30) = happyGoto action_9
action_92 (31) = happyGoto action_10
action_92 (32) = happyGoto action_11
action_92 (36) = happyGoto action_12
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_11

action_94 (53) = happyShift action_131
action_94 (67) = happyShift action_47
action_94 (68) = happyShift action_48
action_94 (69) = happyShift action_49
action_94 (70) = happyShift action_50
action_94 (71) = happyShift action_51
action_94 (72) = happyShift action_52
action_94 (73) = happyShift action_53
action_94 (74) = happyShift action_54
action_94 (75) = happyShift action_55
action_94 (76) = happyShift action_56
action_94 (77) = happyShift action_57
action_94 (78) = happyShift action_58
action_94 _ = happyReduce_50

action_95 (59) = happyShift action_130
action_95 (67) = happyShift action_47
action_95 (68) = happyShift action_48
action_95 (69) = happyShift action_49
action_95 (70) = happyShift action_50
action_95 (71) = happyShift action_51
action_95 (72) = happyShift action_52
action_95 (73) = happyShift action_53
action_95 (74) = happyShift action_54
action_95 (75) = happyShift action_55
action_95 (76) = happyShift action_56
action_95 (77) = happyShift action_57
action_95 (78) = happyShift action_58
action_95 (24) = happyGoto action_129
action_95 _ = happyReduce_47

action_96 (47) = happyShift action_128
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_29

action_98 _ = happyReduce_39

action_99 (38) = happyShift action_127
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (37) = happyShift action_13
action_100 (38) = happyShift action_14
action_100 (39) = happyShift action_15
action_100 (41) = happyShift action_16
action_100 (42) = happyShift action_17
action_100 (44) = happyShift action_18
action_100 (45) = happyShift action_19
action_100 (51) = happyShift action_20
action_100 (56) = happyShift action_21
action_100 (60) = happyShift action_22
action_100 (68) = happyShift action_23
action_100 (5) = happyGoto action_126
action_100 (6) = happyGoto action_3
action_100 (25) = happyGoto action_4
action_100 (26) = happyGoto action_5
action_100 (27) = happyGoto action_6
action_100 (28) = happyGoto action_7
action_100 (29) = happyGoto action_8
action_100 (30) = happyGoto action_9
action_100 (31) = happyGoto action_10
action_100 (32) = happyGoto action_11
action_100 (36) = happyGoto action_12
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (38) = happyShift action_125
action_101 (15) = happyGoto action_123
action_101 (16) = happyGoto action_124
action_101 _ = happyReduce_32

action_102 (38) = happyShift action_120
action_102 (50) = happyShift action_121
action_102 (64) = happyShift action_122
action_102 (21) = happyGoto action_119
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (43) = happyShift action_118
action_103 (67) = happyShift action_47
action_103 (68) = happyShift action_48
action_103 (69) = happyShift action_49
action_103 (70) = happyShift action_50
action_103 (71) = happyShift action_51
action_103 (72) = happyShift action_52
action_103 (73) = happyShift action_53
action_103 (74) = happyShift action_54
action_103 (75) = happyShift action_55
action_103 (76) = happyShift action_56
action_103 (77) = happyShift action_57
action_103 (78) = happyShift action_58
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (67) = happyShift action_47
action_104 (68) = happyShift action_48
action_104 (69) = happyShift action_49
action_104 (70) = happyShift action_50
action_104 (71) = happyShift action_51
action_104 (72) = happyShift action_52
action_104 (73) = happyShift action_53
action_104 (74) = happyShift action_54
action_104 (75) = happyShift action_55
action_104 (76) = happyShift action_56
action_104 (77) = happyShift action_57
action_104 (78) = happyShift action_58
action_104 _ = happyReduce_51

action_105 _ = happyReduce_15

action_106 _ = happyReduce_17

action_107 (37) = happyShift action_13
action_107 (38) = happyShift action_14
action_107 (39) = happyShift action_15
action_107 (41) = happyShift action_16
action_107 (42) = happyShift action_17
action_107 (44) = happyShift action_18
action_107 (45) = happyShift action_19
action_107 (51) = happyShift action_20
action_107 (56) = happyShift action_21
action_107 (60) = happyShift action_22
action_107 (68) = happyShift action_23
action_107 (5) = happyGoto action_117
action_107 (6) = happyGoto action_3
action_107 (25) = happyGoto action_4
action_107 (26) = happyGoto action_5
action_107 (27) = happyGoto action_6
action_107 (28) = happyGoto action_7
action_107 (29) = happyGoto action_8
action_107 (30) = happyGoto action_9
action_107 (31) = happyGoto action_10
action_107 (32) = happyGoto action_11
action_107 (36) = happyGoto action_12
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (55) = happyShift action_116
action_108 _ = happyReduce_60

action_109 (37) = happyShift action_13
action_109 (38) = happyShift action_14
action_109 (39) = happyShift action_15
action_109 (41) = happyShift action_16
action_109 (42) = happyShift action_17
action_109 (44) = happyShift action_18
action_109 (45) = happyShift action_19
action_109 (51) = happyShift action_20
action_109 (56) = happyShift action_21
action_109 (60) = happyShift action_22
action_109 (68) = happyShift action_23
action_109 (5) = happyGoto action_115
action_109 (6) = happyGoto action_3
action_109 (25) = happyGoto action_4
action_109 (26) = happyGoto action_5
action_109 (27) = happyGoto action_6
action_109 (28) = happyGoto action_7
action_109 (29) = happyGoto action_8
action_109 (30) = happyGoto action_9
action_109 (31) = happyGoto action_10
action_109 (32) = happyGoto action_11
action_109 (36) = happyGoto action_12
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_77

action_111 (38) = happyShift action_77
action_111 (34) = happyGoto action_114
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_75

action_113 _ = happyReduce_61

action_114 (57) = happyShift action_111
action_114 (35) = happyGoto action_144
action_114 _ = happyReduce_79

action_115 (67) = happyShift action_47
action_115 (68) = happyShift action_48
action_115 (69) = happyShift action_49
action_115 (70) = happyShift action_50
action_115 (71) = happyShift action_51
action_115 (72) = happyShift action_52
action_115 (73) = happyShift action_53
action_115 (74) = happyShift action_54
action_115 (75) = happyShift action_55
action_115 (76) = happyShift action_56
action_115 (77) = happyShift action_57
action_115 (78) = happyShift action_58
action_115 _ = happyReduce_78

action_116 (37) = happyShift action_13
action_116 (38) = happyShift action_14
action_116 (39) = happyShift action_15
action_116 (41) = happyShift action_16
action_116 (42) = happyShift action_17
action_116 (44) = happyShift action_18
action_116 (45) = happyShift action_19
action_116 (51) = happyShift action_20
action_116 (56) = happyShift action_21
action_116 (60) = happyShift action_22
action_116 (68) = happyShift action_23
action_116 (5) = happyGoto action_143
action_116 (6) = happyGoto action_3
action_116 (25) = happyGoto action_4
action_116 (26) = happyGoto action_5
action_116 (27) = happyGoto action_6
action_116 (28) = happyGoto action_7
action_116 (29) = happyGoto action_8
action_116 (30) = happyGoto action_9
action_116 (31) = happyGoto action_10
action_116 (32) = happyGoto action_11
action_116 (36) = happyGoto action_12
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (57) = happyShift action_107
action_117 (67) = happyShift action_47
action_117 (68) = happyShift action_48
action_117 (69) = happyShift action_49
action_117 (70) = happyShift action_50
action_117 (71) = happyShift action_51
action_117 (72) = happyShift action_52
action_117 (73) = happyShift action_53
action_117 (74) = happyShift action_54
action_117 (75) = happyShift action_55
action_117 (76) = happyShift action_56
action_117 (77) = happyShift action_57
action_117 (78) = happyShift action_58
action_117 (8) = happyGoto action_142
action_117 _ = happyReduce_18

action_118 (37) = happyShift action_13
action_118 (38) = happyShift action_14
action_118 (39) = happyShift action_15
action_118 (41) = happyShift action_16
action_118 (42) = happyShift action_17
action_118 (44) = happyShift action_18
action_118 (45) = happyShift action_19
action_118 (51) = happyShift action_20
action_118 (56) = happyShift action_21
action_118 (60) = happyShift action_22
action_118 (68) = happyShift action_23
action_118 (5) = happyGoto action_141
action_118 (6) = happyGoto action_3
action_118 (25) = happyGoto action_4
action_118 (26) = happyGoto action_5
action_118 (27) = happyGoto action_6
action_118 (28) = happyGoto action_7
action_118 (29) = happyGoto action_8
action_118 (30) = happyGoto action_9
action_118 (31) = happyGoto action_10
action_118 (32) = happyGoto action_11
action_118 (36) = happyGoto action_12
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_40

action_120 _ = happyReduce_41

action_121 (55) = happyShift action_140
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (38) = happyShift action_125
action_122 (15) = happyGoto action_139
action_122 (16) = happyGoto action_124
action_122 _ = happyReduce_32

action_123 (61) = happyShift action_138
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (57) = happyShift action_137
action_124 (17) = happyGoto action_136
action_124 _ = happyReduce_35

action_125 (58) = happyShift action_135
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (67) = happyShift action_47
action_126 (68) = happyShift action_48
action_126 (69) = happyShift action_49
action_126 (70) = happyShift action_50
action_126 (71) = happyShift action_51
action_126 (72) = happyShift action_52
action_126 (73) = happyShift action_53
action_126 (74) = happyShift action_54
action_126 (75) = happyShift action_55
action_126 (76) = happyShift action_56
action_126 (77) = happyShift action_57
action_126 (78) = happyShift action_58
action_126 _ = happyReduce_25

action_127 (79) = happyShift action_134
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_54

action_129 _ = happyReduce_46

action_130 (37) = happyShift action_13
action_130 (38) = happyShift action_14
action_130 (39) = happyShift action_15
action_130 (41) = happyShift action_16
action_130 (42) = happyShift action_17
action_130 (44) = happyShift action_18
action_130 (45) = happyShift action_19
action_130 (51) = happyShift action_20
action_130 (56) = happyShift action_21
action_130 (60) = happyShift action_22
action_130 (68) = happyShift action_23
action_130 (5) = happyGoto action_133
action_130 (6) = happyGoto action_3
action_130 (25) = happyGoto action_4
action_130 (26) = happyGoto action_5
action_130 (27) = happyGoto action_6
action_130 (28) = happyGoto action_7
action_130 (29) = happyGoto action_8
action_130 (30) = happyGoto action_9
action_130 (31) = happyGoto action_10
action_130 (32) = happyGoto action_11
action_130 (36) = happyGoto action_12
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (37) = happyShift action_13
action_131 (38) = happyShift action_14
action_131 (39) = happyShift action_15
action_131 (41) = happyShift action_16
action_131 (42) = happyShift action_17
action_131 (44) = happyShift action_18
action_131 (45) = happyShift action_19
action_131 (51) = happyShift action_20
action_131 (56) = happyShift action_21
action_131 (60) = happyShift action_22
action_131 (68) = happyShift action_23
action_131 (5) = happyGoto action_132
action_131 (6) = happyGoto action_3
action_131 (25) = happyGoto action_4
action_131 (26) = happyGoto action_5
action_131 (27) = happyGoto action_6
action_131 (28) = happyGoto action_7
action_131 (29) = happyGoto action_8
action_131 (30) = happyGoto action_9
action_131 (31) = happyGoto action_10
action_131 (32) = happyGoto action_11
action_131 (36) = happyGoto action_12
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (67) = happyShift action_47
action_132 (68) = happyShift action_48
action_132 (69) = happyShift action_49
action_132 (70) = happyShift action_50
action_132 (71) = happyShift action_51
action_132 (72) = happyShift action_52
action_132 (73) = happyShift action_53
action_132 (74) = happyShift action_54
action_132 (75) = happyShift action_55
action_132 (76) = happyShift action_56
action_132 (77) = happyShift action_57
action_132 (78) = happyShift action_58
action_132 _ = happyReduce_49

action_133 (59) = happyShift action_130
action_133 (67) = happyShift action_47
action_133 (68) = happyShift action_48
action_133 (69) = happyShift action_49
action_133 (70) = happyShift action_50
action_133 (71) = happyShift action_51
action_133 (72) = happyShift action_52
action_133 (73) = happyShift action_53
action_133 (74) = happyShift action_54
action_133 (75) = happyShift action_55
action_133 (76) = happyShift action_56
action_133 (77) = happyShift action_57
action_133 (78) = happyShift action_58
action_133 (24) = happyGoto action_153
action_133 _ = happyReduce_47

action_134 (37) = happyShift action_13
action_134 (38) = happyShift action_14
action_134 (39) = happyShift action_15
action_134 (41) = happyShift action_16
action_134 (42) = happyShift action_17
action_134 (44) = happyShift action_18
action_134 (45) = happyShift action_19
action_134 (51) = happyShift action_20
action_134 (56) = happyShift action_21
action_134 (60) = happyShift action_22
action_134 (68) = happyShift action_23
action_134 (5) = happyGoto action_152
action_134 (6) = happyGoto action_3
action_134 (25) = happyGoto action_4
action_134 (26) = happyGoto action_5
action_134 (27) = happyGoto action_6
action_134 (28) = happyGoto action_7
action_134 (29) = happyGoto action_8
action_134 (30) = happyGoto action_9
action_134 (31) = happyGoto action_10
action_134 (32) = happyGoto action_11
action_134 (36) = happyGoto action_12
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (38) = happyShift action_151
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_33

action_137 (38) = happyShift action_125
action_137 (16) = happyGoto action_150
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (58) = happyShift action_148
action_138 (71) = happyShift action_149
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (65) = happyShift action_147
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (38) = happyShift action_146
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (54) = happyShift action_145
action_141 (67) = happyShift action_47
action_141 (68) = happyShift action_48
action_141 (69) = happyShift action_49
action_141 (70) = happyShift action_50
action_141 (71) = happyShift action_51
action_141 (72) = happyShift action_52
action_141 (73) = happyShift action_53
action_141 (74) = happyShift action_54
action_141 (75) = happyShift action_55
action_141 (76) = happyShift action_56
action_141 (77) = happyShift action_57
action_141 (78) = happyShift action_58
action_141 _ = happyFail (happyExpListPerState 141)

action_142 _ = happyReduce_19

action_143 (67) = happyShift action_47
action_143 (68) = happyShift action_48
action_143 (69) = happyShift action_49
action_143 (70) = happyShift action_50
action_143 (71) = happyShift action_51
action_143 (72) = happyShift action_52
action_143 (73) = happyShift action_53
action_143 (74) = happyShift action_54
action_143 (75) = happyShift action_55
action_143 (76) = happyShift action_56
action_143 (77) = happyShift action_57
action_143 (78) = happyShift action_58
action_143 _ = happyReduce_81

action_144 _ = happyReduce_80

action_145 (37) = happyShift action_13
action_145 (38) = happyShift action_14
action_145 (39) = happyShift action_15
action_145 (41) = happyShift action_16
action_145 (42) = happyShift action_17
action_145 (44) = happyShift action_18
action_145 (45) = happyShift action_19
action_145 (51) = happyShift action_20
action_145 (56) = happyShift action_21
action_145 (60) = happyShift action_22
action_145 (68) = happyShift action_23
action_145 (5) = happyGoto action_157
action_145 (6) = happyGoto action_3
action_145 (25) = happyGoto action_4
action_145 (26) = happyGoto action_5
action_145 (27) = happyGoto action_6
action_145 (28) = happyGoto action_7
action_145 (29) = happyGoto action_8
action_145 (30) = happyGoto action_9
action_145 (31) = happyGoto action_10
action_145 (32) = happyGoto action_11
action_145 (36) = happyGoto action_12
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_43

action_147 _ = happyReduce_42

action_148 (38) = happyShift action_156
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (37) = happyShift action_13
action_149 (38) = happyShift action_14
action_149 (39) = happyShift action_15
action_149 (41) = happyShift action_16
action_149 (42) = happyShift action_17
action_149 (44) = happyShift action_18
action_149 (45) = happyShift action_19
action_149 (51) = happyShift action_20
action_149 (56) = happyShift action_21
action_149 (60) = happyShift action_22
action_149 (68) = happyShift action_23
action_149 (5) = happyGoto action_155
action_149 (6) = happyGoto action_3
action_149 (25) = happyGoto action_4
action_149 (26) = happyGoto action_5
action_149 (27) = happyGoto action_6
action_149 (28) = happyGoto action_7
action_149 (29) = happyGoto action_8
action_149 (30) = happyGoto action_9
action_149 (31) = happyGoto action_10
action_149 (32) = happyGoto action_11
action_149 (36) = happyGoto action_12
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (57) = happyShift action_137
action_150 (17) = happyGoto action_154
action_150 _ = happyReduce_35

action_151 _ = happyReduce_34

action_152 (67) = happyShift action_47
action_152 (68) = happyShift action_48
action_152 (69) = happyShift action_49
action_152 (70) = happyShift action_50
action_152 (71) = happyShift action_51
action_152 (72) = happyShift action_52
action_152 (73) = happyShift action_53
action_152 (74) = happyShift action_54
action_152 (75) = happyShift action_55
action_152 (76) = happyShift action_56
action_152 (77) = happyShift action_57
action_152 (78) = happyShift action_58
action_152 _ = happyReduce_26

action_153 _ = happyReduce_48

action_154 _ = happyReduce_36

action_155 (67) = happyShift action_47
action_155 (68) = happyShift action_48
action_155 (69) = happyShift action_49
action_155 (70) = happyShift action_50
action_155 (71) = happyShift action_51
action_155 (72) = happyShift action_52
action_155 (73) = happyShift action_53
action_155 (74) = happyShift action_54
action_155 (75) = happyShift action_55
action_155 (76) = happyShift action_56
action_155 (77) = happyShift action_57
action_155 (78) = happyShift action_58
action_155 _ = happyReduce_30

action_156 (71) = happyShift action_158
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (67) = happyShift action_47
action_157 (68) = happyShift action_48
action_157 (69) = happyShift action_49
action_157 (70) = happyShift action_50
action_157 (71) = happyShift action_51
action_157 (72) = happyShift action_52
action_157 (73) = happyShift action_53
action_157 (74) = happyShift action_54
action_157 (75) = happyShift action_55
action_157 (76) = happyShift action_56
action_157 (77) = happyShift action_57
action_157 (78) = happyShift action_58
action_157 _ = happyReduce_52

action_158 (37) = happyShift action_13
action_158 (38) = happyShift action_14
action_158 (39) = happyShift action_15
action_158 (41) = happyShift action_16
action_158 (42) = happyShift action_17
action_158 (44) = happyShift action_18
action_158 (45) = happyShift action_19
action_158 (51) = happyShift action_20
action_158 (56) = happyShift action_21
action_158 (60) = happyShift action_22
action_158 (68) = happyShift action_23
action_158 (5) = happyGoto action_159
action_158 (6) = happyGoto action_3
action_158 (25) = happyGoto action_4
action_158 (26) = happyGoto action_5
action_158 (27) = happyGoto action_6
action_158 (28) = happyGoto action_7
action_158 (29) = happyGoto action_8
action_158 (30) = happyGoto action_9
action_158 (31) = happyGoto action_10
action_158 (32) = happyGoto action_11
action_158 (36) = happyGoto action_12
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (67) = happyShift action_47
action_159 (68) = happyShift action_48
action_159 (69) = happyShift action_49
action_159 (70) = happyShift action_50
action_159 (71) = happyShift action_51
action_159 (72) = happyShift action_52
action_159 (73) = happyShift action_53
action_159 (74) = happyShift action_54
action_159 (75) = happyShift action_55
action_159 (76) = happyShift action_56
action_159 (77) = happyShift action_57
action_159 (78) = happyShift action_58
action_159 _ = happyReduce_31

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn4
		 (Var happy_var_1 (sp happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let L.Tok l (L.LitInt i) = happy_var_1 in Num i ()
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (let L.Tok l (L.LitString s) = happy_var_1 in Str s ()
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 (HappyTerminal (L.Tok happy_var_1  L.Nil))
	 =  HappyAbsSyn4
		 (Nil (sp happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 6 happyReduction_15
happyReduction_15 ((HappyTerminal (L.Tok happy_var_4  L.RParen)) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Call (newSym happy_var_1) happy_var_3 ((sp happy_var_1)<> (sp happy_var_4))
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  7 happyReduction_16
happyReduction_16  =  HappyAbsSyn7
		 ([]
	)

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  8 happyReduction_18
happyReduction_18  =  HappyAbsSyn7
		 ([]
	)

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  9 happyReduction_20
happyReduction_20  =  HappyAbsSyn9
		 ([]
	)

happyReduce_21 = happySpecReduce_2  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (FunDecl happy_var_1 (foldMap sp happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  10 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn10
		 (TypeDecl     happy_var_1 (foldMap (\ (_,_,x) -> x) happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 11 happyReduction_25
happyReduction_25 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Var)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (VarDecl (newSym happy_var_2) Nothing happy_var_4 (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 6 11 happyReduction_26
happyReduction_26 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Var)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (VarDecl (newSym happy_var_2) (Just (newSym happy_var_4, sp happy_var_4)) happy_var_6 (spr happy_var_1 happy_var_6)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  13 happyReduction_28
happyReduction_28  =  HappyAbsSyn12
		 ([]
	)

happyReduce_29 = happySpecReduce_2  13 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 7 14 happyReduction_30
happyReduction_30 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Function)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Function (newSym happy_var_2) happy_var_4 Nothing      happy_var_7 (spr happy_var_1 happy_var_7)
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 9 14 happyReduction_31
happyReduction_31 ((HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Function)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Function (newSym happy_var_2) happy_var_4 (Just (newSym happy_var_7, sp happy_var_7)) happy_var_9 (spr happy_var_1 happy_var_9)
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_0  15 happyReduction_32
happyReduction_32  =  HappyAbsSyn15
		 ([]
	)

happyReduce_33 = happySpecReduce_2  15 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (Field (newSym happy_var_1) (newSym happy_var_3) (spr happy_var_1 happy_var_3)
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  17 happyReduction_35
happyReduction_35  =  HappyAbsSyn15
		 ([]
	)

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  19 happyReduction_38
happyReduction_38  =  HappyAbsSyn18
		 ([]
	)

happyReduce_39 = happySpecReduce_2  19 happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 20 happyReduction_40
happyReduction_40 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Type)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((newSym happy_var_2, happy_var_4, (spr happy_var_1 happy_var_4))
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  21 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (NameTy (newSym happy_var_1) (sp happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyTerminal (L.Tok happy_var_3  L.RBrace))
	(HappyAbsSyn15  happy_var_2)
	(HappyTerminal (L.Tok happy_var_1  L.LBrace))
	 =  HappyAbsSyn21
		 (RecordTy happy_var_2 (spr happy_var_1 happy_var_3)
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_3)
	_
	(HappyTerminal (L.Tok happy_var_1  L.Array))
	 =  HappyAbsSyn21
		 (ArrayTy (newSym happy_var_3) (spr happy_var_1 happy_var_3)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  22 happyReduction_44
happyReduction_44 (HappyTerminal (L.Tok happy_var_3  L.RParen))
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal (L.Tok happy_var_1  L.LParen))
	 =  HappyAbsSyn4
		 (Seq happy_var_2 (sp happy_var_1 <> sp happy_var_3)
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  23 happyReduction_45
happyReduction_45  =  HappyAbsSyn7
		 ([]
	)

happyReduce_46 = happySpecReduce_2  23 happyReduction_46
happyReduction_46 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  24 happyReduction_47
happyReduction_47  =  HappyAbsSyn7
		 ([]
	)

happyReduce_48 = happySpecReduce_3  24 happyReduction_48
happyReduction_48 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 6 25 happyReduction_49
happyReduction_49 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.If)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 (Just happy_var_6) (spr happy_var_1 happy_var_6)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 25 happyReduction_50
happyReduction_50 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.If)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4  Nothing      (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 4 25 happyReduction_51
happyReduction_51 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.While)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4            (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 8 25 happyReduction_52
happyReduction_52 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.For)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (For (newSym happy_var_2) happy_var_4 happy_var_6 happy_var_8  (spr happy_var_1 happy_var_8)
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_1  25 happyReduction_53
happyReduction_53 (HappyTerminal (L.Tok happy_var_1  L.Break))
	 =  HappyAbsSyn4
		 (Break                  (sp happy_var_1)
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happyReduce 5 25 happyReduction_54
happyReduction_54 ((HappyTerminal (L.Tok happy_var_5  L.End)) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyTerminal (L.Tok happy_var_1  L.Let)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 (Seq happy_var_4 (foldMap sp happy_var_4)) (spr happy_var_1 happy_var_5)
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_3  26 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (SimpleVar (newSym happy_var_1) (sp happy_var_1)
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  28 happyReduction_58
happyReduction_58 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (FieldVar (SimpleVar (newSym happy_var_1) (sp happy_var_1)) (newSym happy_var_3) (spr happy_var_1 happy_var_3)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  28 happyReduction_59
happyReduction_59 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (FieldVar happy_var_1 (newSym happy_var_3) (spr happy_var_1 happy_var_3)
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 28 happyReduction_60
happyReduction_60 ((HappyTerminal (L.Tok happy_var_4  L.RBracket)) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (SubsVar (SimpleVar (newSym happy_var_1) (sp happy_var_1)) happy_var_3 (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_61 = happyReduce 4 28 happyReduction_61
happyReduction_61 ((HappyTerminal (L.Tok happy_var_4  L.RBracket)) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (SubsVar happy_var_1 happy_var_3 (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_3  29 happyReduction_62
happyReduction_62 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Eq      happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  29 happyReduction_63
happyReduction_63 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Neq     happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  29 happyReduction_64
happyReduction_64 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Lt      happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  29 happyReduction_65
happyReduction_65 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Gt      happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  29 happyReduction_66
happyReduction_66 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Le      happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  29 happyReduction_67
happyReduction_67 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Ge      happy_var_3 (spr happy_var_1 happy_var_3)
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  30 happyReduction_68
happyReduction_68 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal (L.Tok happy_var_1  L.TMinus))
	 =  HappyAbsSyn4
		 (Op (Num (-1) mempty) Times happy_var_2 (spr happy_var_1 happy_var_2)
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  30 happyReduction_69
happyReduction_69 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Plus   happy_var_3  (spr happy_var_1 happy_var_3)
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  30 happyReduction_70
happyReduction_70 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Minus  happy_var_3  (spr happy_var_1 happy_var_3)
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  30 happyReduction_71
happyReduction_71 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Times  happy_var_3  (spr happy_var_1 happy_var_3)
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  30 happyReduction_72
happyReduction_72 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Op happy_var_1 Divide happy_var_3  (spr happy_var_1 happy_var_3)
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  31 happyReduction_73
happyReduction_73 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (If happy_var_1 happy_var_3 (Just (Num 0 mempty)) (spr happy_var_1 happy_var_3)
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  31 happyReduction_74
happyReduction_74 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (If happy_var_1 (Num 1 mempty) (Just happy_var_3) (spr happy_var_1 happy_var_3)
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 32 happyReduction_75
happyReduction_75 ((HappyTerminal (L.Tok happy_var_4  L.RBrace)) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Record  (newSym happy_var_1) happy_var_3 (spr happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_0  33 happyReduction_76
happyReduction_76  =  HappyAbsSyn33
		 ([]
	)

happyReduce_77 = happySpecReduce_2  33 happyReduction_77
happyReduction_77 (HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  34 happyReduction_78
happyReduction_78 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 ((newSym happy_var_1, happy_var_3, (spr happy_var_1 happy_var_3))
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_0  35 happyReduction_79
happyReduction_79  =  HappyAbsSyn33
		 ([]
	)

happyReduce_80 = happySpecReduce_3  35 happyReduction_80
happyReduction_80 (HappyAbsSyn33  happy_var_3)
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2 : happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happyReduce 6 36 happyReduction_81
happyReduction_81 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Array (newSym happy_var_1) happy_var_3 happy_var_6  (spr happy_var_1 happy_var_6)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 80 80 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.Tok _  (L.LitInt _) -> cont 37;
	L.Tok _  (L.Ident _ ) -> cont 38;
	L.Tok _  (L.LitString _ ) -> cont 39;
	L.Tok happy_dollar_dollar  L.Var -> cont 40;
	L.Tok happy_dollar_dollar  L.While -> cont 41;
	L.Tok happy_dollar_dollar  L.For -> cont 42;
	L.Tok happy_dollar_dollar  L.To -> cont 43;
	L.Tok happy_dollar_dollar  L.Break -> cont 44;
	L.Tok happy_dollar_dollar  L.Let -> cont 45;
	L.Tok happy_dollar_dollar  L.In -> cont 46;
	L.Tok happy_dollar_dollar  L.End -> cont 47;
	L.Tok happy_dollar_dollar  L.Function -> cont 48;
	L.Tok happy_dollar_dollar  L.Type -> cont 49;
	L.Tok happy_dollar_dollar  L.Array -> cont 50;
	L.Tok happy_dollar_dollar  L.If -> cont 51;
	L.Tok happy_dollar_dollar  L.Then -> cont 52;
	L.Tok happy_dollar_dollar  L.Else -> cont 53;
	L.Tok happy_dollar_dollar  L.Do -> cont 54;
	L.Tok happy_dollar_dollar  L.Of -> cont 55;
	L.Tok happy_dollar_dollar  L.Nil -> cont 56;
	L.Tok happy_dollar_dollar  L.Comma -> cont 57;
	L.Tok happy_dollar_dollar  L.Colon -> cont 58;
	L.Tok happy_dollar_dollar  L.Semicolon -> cont 59;
	L.Tok happy_dollar_dollar  L.LParen -> cont 60;
	L.Tok happy_dollar_dollar  L.RParen -> cont 61;
	L.Tok happy_dollar_dollar  L.LBracket -> cont 62;
	L.Tok happy_dollar_dollar  L.RBracket -> cont 63;
	L.Tok happy_dollar_dollar  L.LBrace -> cont 64;
	L.Tok happy_dollar_dollar  L.RBrace -> cont 65;
	L.Tok happy_dollar_dollar  L.Dot -> cont 66;
	L.Tok happy_dollar_dollar  L.TPlus -> cont 67;
	L.Tok happy_dollar_dollar  L.TMinus -> cont 68;
	L.Tok happy_dollar_dollar  L.TMul -> cont 69;
	L.Tok happy_dollar_dollar  L.TDiv -> cont 70;
	L.Tok happy_dollar_dollar  L.Equal -> cont 71;
	L.Tok happy_dollar_dollar  L.NotEqual -> cont 72;
	L.Tok happy_dollar_dollar  L.GreaterThan -> cont 73;
	L.Tok happy_dollar_dollar  L.LessThan -> cont 74;
	L.Tok happy_dollar_dollar  L.GreaterThanEqual -> cont 75;
	L.Tok happy_dollar_dollar  L.LessThanEqual -> cont 76;
	L.Tok happy_dollar_dollar  L.And -> cont 77;
	L.Tok happy_dollar_dollar  L.Or -> cont 78;
	L.Tok happy_dollar_dollar  L.Assign -> cont 79;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 80 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = ((>>=))
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> P a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(L.Tok)], [String]) -> P a
happyError' = (\(tokens, _) -> parseError tokens)
program tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type P = Either String
type L = ()
parseError :: [L.Tok] -> P a
parseError toks = Left $ "A parse error occurred\n\n " <> show toks

spr _ _ = ()


newSym (Tok _ (L.Ident s)) = Symbol s

sp _ = ()

parseProgram :: String -> Either String (Expr L)
parseProgram = program <=< L.mylexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
