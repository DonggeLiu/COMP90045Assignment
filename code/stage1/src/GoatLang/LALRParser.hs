{-# OPTIONS_GHC -w #-}
module GoatLang.LALRParser where
-- module Main (main, parse) where   -- for standalone testing

import GoatLang.Lexer
import GoatLang.AST

import Data.List (intersperse)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (GoatProgram)
	| HappyAbsSyn5 ([Proc])
	| HappyAbsSyn6 (Proc)
	| HappyAbsSyn7 ([Param])
	| HappyAbsSyn9 (Param)
	| HappyAbsSyn10 (PassBy)
	| HappyAbsSyn11 (BaseType)
	| HappyAbsSyn12 ([Decl])
	| HappyAbsSyn13 (Decl)
	| HappyAbsSyn14 (Dim)
	| HappyAbsSyn15 ([Stmt])
	| HappyAbsSyn16 (Stmt)
	| HappyAbsSyn17 (Var)
	| HappyAbsSyn18 ([Expr])
	| HappyAbsSyn20 (Expr)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
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
 action_116 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_56 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,309) ([0,32768,0,0,0,32768,0,0,0,0,0,0,0,32768,0,0,0,0,0,16384,0,0,0,0,0,0,128,0,0,0,0,0,0,0,10,0,0,0,256,0,0,0,0,0,0,0,2048,0,0,10272,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,10272,0,0,0,0,0,16384,0,16,0,0,0,10272,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4160,49,16384,0,0,512,0,0,0,4096,0,0,0,0,4096,0,512,0,0,0,4160,49,16384,0,0,64,0,0,0,0,16384,0,0,16512,32256,0,0,0,16384,0,0,16512,32256,0,0,16512,32256,0,0,512,0,0,0,16512,32256,0,0,0,0,0,0,61440,511,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,57344,511,0,0,4096,0,0,0,57348,511,0,0,128,0,0,0,16512,32256,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,512,0,0,0,61440,511,0,0,16512,32256,0,4160,49,16384,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,16512,32256,0,0,0,0,0,4160,49,16384,0,0,57344,127,0,0,0,0,0,0,57600,511,0,0,0,0,0,0,58368,511,0,0,512,0,0,0,0,0,0,16384,0,0,0,0,57344,255,0,0,57344,127,0,0,57344,1,0,0,57344,1,0,0,57344,1,0,0,57344,1,0,0,57344,1,0,0,57344,1,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,32768,1,0,1280,0,0,0,0,256,0,0,0,0,0,0,0,59392,511,0,0,0,0,0,0,0,4096,0,0,1024,0,0,0,16512,32256,0,0,4096,0,0,4160,49,16384,0,0,0,0,0,0,0,0,0,0,16512,32256,0,0,58368,511,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_p","Goat","Procs","Proc","Params","Params1","Param","PassBy","BaseType","Decls","Decl","Dim","Stmts","Stmt","Var","Exprs","Exprs1","Expr","key_begin","key_bool","key_call","key_do","key_else","key_end","key_fi","key_float","key_if","key_int","key_od","key_proc","key_read","key_ref","key_then","key_val","key_while","key_write","asg","'('","')'","'['","']'","','","';'","'+'","'-'","'*'","'/'","'='","neq","'<'","lte","'>'","gte","and","or","'!'","boolconst","fracconst","intconst","strconst","ident","%eof"]
        bit_start = st * 64
        bit_end = (st + 1) * 64
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..63]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (32) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (32) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (32) = happyShift action_4
action_3 (5) = happyGoto action_7
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (63) = happyShift action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (64) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (40) = happyShift action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_3

action_8 (34) = happyShift action_13
action_8 (36) = happyShift action_14
action_8 (7) = happyGoto action_9
action_8 (8) = happyGoto action_10
action_8 (9) = happyGoto action_11
action_8 (10) = happyGoto action_12
action_8 _ = happyReduce_5

action_9 (41) = happyShift action_20
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_6

action_11 (44) = happyShift action_19
action_11 _ = happyReduce_7

action_12 (22) = happyShift action_16
action_12 (28) = happyShift action_17
action_12 (30) = happyShift action_18
action_12 (11) = happyGoto action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_11

action_14 _ = happyReduce_10

action_15 (63) = happyShift action_25
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_12

action_17 _ = happyReduce_13

action_18 _ = happyReduce_14

action_19 (34) = happyShift action_13
action_19 (36) = happyShift action_14
action_19 (8) = happyGoto action_24
action_19 (9) = happyGoto action_11
action_19 (10) = happyGoto action_12
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (22) = happyShift action_16
action_20 (28) = happyShift action_17
action_20 (30) = happyShift action_18
action_20 (11) = happyGoto action_21
action_20 (12) = happyGoto action_22
action_20 (13) = happyGoto action_23
action_20 _ = happyReduce_15

action_21 (63) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (21) = happyShift action_27
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (22) = happyShift action_16
action_23 (28) = happyShift action_17
action_23 (30) = happyShift action_18
action_23 (11) = happyGoto action_21
action_23 (12) = happyGoto action_26
action_23 (13) = happyGoto action_23
action_23 _ = happyReduce_15

action_24 _ = happyReduce_8

action_25 _ = happyReduce_9

action_26 _ = happyReduce_16

action_27 (23) = happyShift action_34
action_27 (29) = happyShift action_35
action_27 (33) = happyShift action_36
action_27 (37) = happyShift action_37
action_27 (38) = happyShift action_38
action_27 (63) = happyShift action_39
action_27 (15) = happyGoto action_31
action_27 (16) = happyGoto action_32
action_27 (17) = happyGoto action_33
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (42) = happyShift action_30
action_28 (14) = happyGoto action_29
action_28 _ = happyReduce_18

action_29 (45) = happyShift action_58
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (61) = happyShift action_57
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (26) = happyShift action_56
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (23) = happyShift action_34
action_32 (29) = happyShift action_35
action_32 (33) = happyShift action_36
action_32 (37) = happyShift action_37
action_32 (38) = happyShift action_38
action_32 (63) = happyShift action_39
action_32 (15) = happyGoto action_55
action_32 (16) = happyGoto action_32
action_32 (17) = happyGoto action_33
action_32 _ = happyReduce_21

action_33 (39) = happyShift action_54
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (63) = happyShift action_53
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (40) = happyShift action_43
action_35 (47) = happyShift action_44
action_35 (58) = happyShift action_45
action_35 (59) = happyShift action_46
action_35 (60) = happyShift action_47
action_35 (61) = happyShift action_48
action_35 (62) = happyShift action_49
action_35 (63) = happyShift action_39
action_35 (17) = happyGoto action_41
action_35 (20) = happyGoto action_52
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (63) = happyShift action_39
action_36 (17) = happyGoto action_51
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (40) = happyShift action_43
action_37 (47) = happyShift action_44
action_37 (58) = happyShift action_45
action_37 (59) = happyShift action_46
action_37 (60) = happyShift action_47
action_37 (61) = happyShift action_48
action_37 (62) = happyShift action_49
action_37 (63) = happyShift action_39
action_37 (17) = happyGoto action_41
action_37 (20) = happyGoto action_50
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (40) = happyShift action_43
action_38 (47) = happyShift action_44
action_38 (58) = happyShift action_45
action_38 (59) = happyShift action_46
action_38 (60) = happyShift action_47
action_38 (61) = happyShift action_48
action_38 (62) = happyShift action_49
action_38 (63) = happyShift action_39
action_38 (17) = happyGoto action_41
action_38 (20) = happyGoto action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (42) = happyShift action_40
action_39 _ = happyReduce_30

action_40 (40) = happyShift action_43
action_40 (47) = happyShift action_44
action_40 (58) = happyShift action_45
action_40 (59) = happyShift action_46
action_40 (60) = happyShift action_47
action_40 (61) = happyShift action_48
action_40 (62) = happyShift action_49
action_40 (63) = happyShift action_39
action_40 (17) = happyGoto action_41
action_40 (20) = happyGoto action_81
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_37

action_42 (45) = happyShift action_80
action_42 (46) = happyShift action_63
action_42 (47) = happyShift action_64
action_42 (48) = happyShift action_65
action_42 (49) = happyShift action_66
action_42 (50) = happyShift action_67
action_42 (51) = happyShift action_68
action_42 (52) = happyShift action_69
action_42 (53) = happyShift action_70
action_42 (54) = happyShift action_71
action_42 (55) = happyShift action_72
action_42 (56) = happyShift action_73
action_42 (57) = happyShift action_74
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (40) = happyShift action_43
action_43 (47) = happyShift action_44
action_43 (58) = happyShift action_45
action_43 (59) = happyShift action_46
action_43 (60) = happyShift action_47
action_43 (61) = happyShift action_48
action_43 (62) = happyShift action_49
action_43 (63) = happyShift action_39
action_43 (17) = happyGoto action_41
action_43 (20) = happyGoto action_79
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (40) = happyShift action_43
action_44 (47) = happyShift action_44
action_44 (58) = happyShift action_45
action_44 (59) = happyShift action_46
action_44 (60) = happyShift action_47
action_44 (61) = happyShift action_48
action_44 (62) = happyShift action_49
action_44 (63) = happyShift action_39
action_44 (17) = happyGoto action_41
action_44 (20) = happyGoto action_78
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (40) = happyShift action_43
action_45 (47) = happyShift action_44
action_45 (58) = happyShift action_45
action_45 (59) = happyShift action_46
action_45 (60) = happyShift action_47
action_45 (61) = happyShift action_48
action_45 (62) = happyShift action_49
action_45 (63) = happyShift action_39
action_45 (17) = happyGoto action_41
action_45 (20) = happyGoto action_77
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_38

action_47 _ = happyReduce_39

action_48 _ = happyReduce_40

action_49 _ = happyReduce_41

action_50 (24) = happyShift action_76
action_50 (46) = happyShift action_63
action_50 (47) = happyShift action_64
action_50 (48) = happyShift action_65
action_50 (49) = happyShift action_66
action_50 (50) = happyShift action_67
action_50 (51) = happyShift action_68
action_50 (52) = happyShift action_69
action_50 (53) = happyShift action_70
action_50 (54) = happyShift action_71
action_50 (55) = happyShift action_72
action_50 (56) = happyShift action_73
action_50 (57) = happyShift action_74
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (45) = happyShift action_75
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (35) = happyShift action_62
action_52 (46) = happyShift action_63
action_52 (47) = happyShift action_64
action_52 (48) = happyShift action_65
action_52 (49) = happyShift action_66
action_52 (50) = happyShift action_67
action_52 (51) = happyShift action_68
action_52 (52) = happyShift action_69
action_52 (53) = happyShift action_70
action_52 (54) = happyShift action_71
action_52 (55) = happyShift action_72
action_52 (56) = happyShift action_73
action_52 (57) = happyShift action_74
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (40) = happyShift action_61
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (40) = happyShift action_43
action_54 (47) = happyShift action_44
action_54 (58) = happyShift action_45
action_54 (59) = happyShift action_46
action_54 (60) = happyShift action_47
action_54 (61) = happyShift action_48
action_54 (62) = happyShift action_49
action_54 (63) = happyShift action_39
action_54 (17) = happyGoto action_41
action_54 (20) = happyGoto action_60
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_22

action_56 _ = happyReduce_4

action_57 (43) = happyShift action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_17

action_59 (42) = happyShift action_102
action_59 _ = happyReduce_19

action_60 (45) = happyShift action_101
action_60 (46) = happyShift action_63
action_60 (47) = happyShift action_64
action_60 (48) = happyShift action_65
action_60 (49) = happyShift action_66
action_60 (50) = happyShift action_67
action_60 (51) = happyShift action_68
action_60 (52) = happyShift action_69
action_60 (53) = happyShift action_70
action_60 (54) = happyShift action_71
action_60 (55) = happyShift action_72
action_60 (56) = happyShift action_73
action_60 (57) = happyShift action_74
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (40) = happyShift action_43
action_61 (47) = happyShift action_44
action_61 (58) = happyShift action_45
action_61 (59) = happyShift action_46
action_61 (60) = happyShift action_47
action_61 (61) = happyShift action_48
action_61 (62) = happyShift action_49
action_61 (63) = happyShift action_39
action_61 (17) = happyGoto action_41
action_61 (18) = happyGoto action_98
action_61 (19) = happyGoto action_99
action_61 (20) = happyGoto action_100
action_61 _ = happyReduce_33

action_62 (23) = happyShift action_34
action_62 (29) = happyShift action_35
action_62 (33) = happyShift action_36
action_62 (37) = happyShift action_37
action_62 (38) = happyShift action_38
action_62 (63) = happyShift action_39
action_62 (15) = happyGoto action_97
action_62 (16) = happyGoto action_32
action_62 (17) = happyGoto action_33
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (40) = happyShift action_43
action_63 (47) = happyShift action_44
action_63 (58) = happyShift action_45
action_63 (59) = happyShift action_46
action_63 (60) = happyShift action_47
action_63 (61) = happyShift action_48
action_63 (62) = happyShift action_49
action_63 (63) = happyShift action_39
action_63 (17) = happyGoto action_41
action_63 (20) = happyGoto action_96
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (40) = happyShift action_43
action_64 (47) = happyShift action_44
action_64 (58) = happyShift action_45
action_64 (59) = happyShift action_46
action_64 (60) = happyShift action_47
action_64 (61) = happyShift action_48
action_64 (62) = happyShift action_49
action_64 (63) = happyShift action_39
action_64 (17) = happyGoto action_41
action_64 (20) = happyGoto action_95
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (40) = happyShift action_43
action_65 (47) = happyShift action_44
action_65 (58) = happyShift action_45
action_65 (59) = happyShift action_46
action_65 (60) = happyShift action_47
action_65 (61) = happyShift action_48
action_65 (62) = happyShift action_49
action_65 (63) = happyShift action_39
action_65 (17) = happyGoto action_41
action_65 (20) = happyGoto action_94
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (40) = happyShift action_43
action_66 (47) = happyShift action_44
action_66 (58) = happyShift action_45
action_66 (59) = happyShift action_46
action_66 (60) = happyShift action_47
action_66 (61) = happyShift action_48
action_66 (62) = happyShift action_49
action_66 (63) = happyShift action_39
action_66 (17) = happyGoto action_41
action_66 (20) = happyGoto action_93
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (40) = happyShift action_43
action_67 (47) = happyShift action_44
action_67 (58) = happyShift action_45
action_67 (59) = happyShift action_46
action_67 (60) = happyShift action_47
action_67 (61) = happyShift action_48
action_67 (62) = happyShift action_49
action_67 (63) = happyShift action_39
action_67 (17) = happyGoto action_41
action_67 (20) = happyGoto action_92
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (40) = happyShift action_43
action_68 (47) = happyShift action_44
action_68 (58) = happyShift action_45
action_68 (59) = happyShift action_46
action_68 (60) = happyShift action_47
action_68 (61) = happyShift action_48
action_68 (62) = happyShift action_49
action_68 (63) = happyShift action_39
action_68 (17) = happyGoto action_41
action_68 (20) = happyGoto action_91
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (40) = happyShift action_43
action_69 (47) = happyShift action_44
action_69 (58) = happyShift action_45
action_69 (59) = happyShift action_46
action_69 (60) = happyShift action_47
action_69 (61) = happyShift action_48
action_69 (62) = happyShift action_49
action_69 (63) = happyShift action_39
action_69 (17) = happyGoto action_41
action_69 (20) = happyGoto action_90
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (40) = happyShift action_43
action_70 (47) = happyShift action_44
action_70 (58) = happyShift action_45
action_70 (59) = happyShift action_46
action_70 (60) = happyShift action_47
action_70 (61) = happyShift action_48
action_70 (62) = happyShift action_49
action_70 (63) = happyShift action_39
action_70 (17) = happyGoto action_41
action_70 (20) = happyGoto action_89
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (40) = happyShift action_43
action_71 (47) = happyShift action_44
action_71 (58) = happyShift action_45
action_71 (59) = happyShift action_46
action_71 (60) = happyShift action_47
action_71 (61) = happyShift action_48
action_71 (62) = happyShift action_49
action_71 (63) = happyShift action_39
action_71 (17) = happyGoto action_41
action_71 (20) = happyGoto action_88
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (40) = happyShift action_43
action_72 (47) = happyShift action_44
action_72 (58) = happyShift action_45
action_72 (59) = happyShift action_46
action_72 (60) = happyShift action_47
action_72 (61) = happyShift action_48
action_72 (62) = happyShift action_49
action_72 (63) = happyShift action_39
action_72 (17) = happyGoto action_41
action_72 (20) = happyGoto action_87
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (40) = happyShift action_43
action_73 (47) = happyShift action_44
action_73 (58) = happyShift action_45
action_73 (59) = happyShift action_46
action_73 (60) = happyShift action_47
action_73 (61) = happyShift action_48
action_73 (62) = happyShift action_49
action_73 (63) = happyShift action_39
action_73 (17) = happyGoto action_41
action_73 (20) = happyGoto action_86
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (40) = happyShift action_43
action_74 (47) = happyShift action_44
action_74 (58) = happyShift action_45
action_74 (59) = happyShift action_46
action_74 (60) = happyShift action_47
action_74 (61) = happyShift action_48
action_74 (62) = happyShift action_49
action_74 (63) = happyShift action_39
action_74 (17) = happyGoto action_41
action_74 (20) = happyGoto action_85
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_24

action_76 (23) = happyShift action_34
action_76 (29) = happyShift action_35
action_76 (33) = happyShift action_36
action_76 (37) = happyShift action_37
action_76 (38) = happyShift action_38
action_76 (63) = happyShift action_39
action_76 (15) = happyGoto action_84
action_76 (16) = happyGoto action_32
action_76 (17) = happyGoto action_33
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (46) = happyShift action_63
action_77 (47) = happyShift action_64
action_77 (48) = happyShift action_65
action_77 (49) = happyShift action_66
action_77 (50) = happyShift action_67
action_77 (51) = happyShift action_68
action_77 (52) = happyShift action_69
action_77 (53) = happyShift action_70
action_77 (54) = happyShift action_71
action_77 (55) = happyShift action_72
action_77 _ = happyReduce_55

action_78 _ = happyReduce_56

action_79 (41) = happyShift action_83
action_79 (46) = happyShift action_63
action_79 (47) = happyShift action_64
action_79 (48) = happyShift action_65
action_79 (49) = happyShift action_66
action_79 (50) = happyShift action_67
action_79 (51) = happyShift action_68
action_79 (52) = happyShift action_69
action_79 (53) = happyShift action_70
action_79 (54) = happyShift action_71
action_79 (55) = happyShift action_72
action_79 (56) = happyShift action_73
action_79 (57) = happyShift action_74
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_25

action_81 (43) = happyShift action_82
action_81 (46) = happyShift action_63
action_81 (47) = happyShift action_64
action_81 (48) = happyShift action_65
action_81 (49) = happyShift action_66
action_81 (50) = happyShift action_67
action_81 (51) = happyShift action_68
action_81 (52) = happyShift action_69
action_81 (53) = happyShift action_70
action_81 (54) = happyShift action_71
action_81 (55) = happyShift action_72
action_81 (56) = happyShift action_73
action_81 (57) = happyShift action_74
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (42) = happyShift action_109
action_82 _ = happyReduce_31

action_83 _ = happyReduce_42

action_84 (31) = happyShift action_108
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (46) = happyShift action_63
action_85 (47) = happyShift action_64
action_85 (48) = happyShift action_65
action_85 (49) = happyShift action_66
action_85 (50) = happyShift action_67
action_85 (51) = happyShift action_68
action_85 (52) = happyShift action_69
action_85 (53) = happyShift action_70
action_85 (54) = happyShift action_71
action_85 (55) = happyShift action_72
action_85 (56) = happyShift action_73
action_85 _ = happyReduce_54

action_86 (46) = happyShift action_63
action_86 (47) = happyShift action_64
action_86 (48) = happyShift action_65
action_86 (49) = happyShift action_66
action_86 (50) = happyShift action_67
action_86 (51) = happyShift action_68
action_86 (52) = happyShift action_69
action_86 (53) = happyShift action_70
action_86 (54) = happyShift action_71
action_86 (55) = happyShift action_72
action_86 _ = happyReduce_53

action_87 (46) = happyShift action_63
action_87 (47) = happyShift action_64
action_87 (48) = happyShift action_65
action_87 (49) = happyShift action_66
action_87 (50) = happyFail []
action_87 (51) = happyFail []
action_87 (52) = happyFail []
action_87 (53) = happyFail []
action_87 (54) = happyFail []
action_87 (55) = happyFail []
action_87 _ = happyReduce_52

action_88 (46) = happyShift action_63
action_88 (47) = happyShift action_64
action_88 (48) = happyShift action_65
action_88 (49) = happyShift action_66
action_88 (50) = happyFail []
action_88 (51) = happyFail []
action_88 (52) = happyFail []
action_88 (53) = happyFail []
action_88 (54) = happyFail []
action_88 (55) = happyFail []
action_88 _ = happyReduce_51

action_89 (46) = happyShift action_63
action_89 (47) = happyShift action_64
action_89 (48) = happyShift action_65
action_89 (49) = happyShift action_66
action_89 (50) = happyFail []
action_89 (51) = happyFail []
action_89 (52) = happyFail []
action_89 (53) = happyFail []
action_89 (54) = happyFail []
action_89 (55) = happyFail []
action_89 _ = happyReduce_50

action_90 (46) = happyShift action_63
action_90 (47) = happyShift action_64
action_90 (48) = happyShift action_65
action_90 (49) = happyShift action_66
action_90 (50) = happyFail []
action_90 (51) = happyFail []
action_90 (52) = happyFail []
action_90 (53) = happyFail []
action_90 (54) = happyFail []
action_90 (55) = happyFail []
action_90 _ = happyReduce_49

action_91 (46) = happyShift action_63
action_91 (47) = happyShift action_64
action_91 (48) = happyShift action_65
action_91 (49) = happyShift action_66
action_91 (50) = happyFail []
action_91 (51) = happyFail []
action_91 (52) = happyFail []
action_91 (53) = happyFail []
action_91 (54) = happyFail []
action_91 (55) = happyFail []
action_91 _ = happyReduce_48

action_92 (46) = happyShift action_63
action_92 (47) = happyShift action_64
action_92 (48) = happyShift action_65
action_92 (49) = happyShift action_66
action_92 (50) = happyFail []
action_92 (51) = happyFail []
action_92 (52) = happyFail []
action_92 (53) = happyFail []
action_92 (54) = happyFail []
action_92 (55) = happyFail []
action_92 _ = happyReduce_47

action_93 _ = happyReduce_46

action_94 _ = happyReduce_45

action_95 (48) = happyShift action_65
action_95 (49) = happyShift action_66
action_95 _ = happyReduce_44

action_96 (48) = happyShift action_65
action_96 (49) = happyShift action_66
action_96 _ = happyReduce_43

action_97 (25) = happyShift action_106
action_97 (27) = happyShift action_107
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (41) = happyShift action_105
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_34

action_100 (44) = happyShift action_104
action_100 (46) = happyShift action_63
action_100 (47) = happyShift action_64
action_100 (48) = happyShift action_65
action_100 (49) = happyShift action_66
action_100 (50) = happyShift action_67
action_100 (51) = happyShift action_68
action_100 (52) = happyShift action_69
action_100 (53) = happyShift action_70
action_100 (54) = happyShift action_71
action_100 (55) = happyShift action_72
action_100 (56) = happyShift action_73
action_100 (57) = happyShift action_74
action_100 _ = happyReduce_35

action_101 _ = happyReduce_23

action_102 (61) = happyShift action_103
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (43) = happyShift action_114
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (40) = happyShift action_43
action_104 (47) = happyShift action_44
action_104 (58) = happyShift action_45
action_104 (59) = happyShift action_46
action_104 (60) = happyShift action_47
action_104 (61) = happyShift action_48
action_104 (62) = happyShift action_49
action_104 (63) = happyShift action_39
action_104 (17) = happyGoto action_41
action_104 (18) = happyGoto action_113
action_104 (19) = happyGoto action_99
action_104 (20) = happyGoto action_100
action_104 _ = happyReduce_33

action_105 (45) = happyShift action_112
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (23) = happyShift action_34
action_106 (29) = happyShift action_35
action_106 (33) = happyShift action_36
action_106 (37) = happyShift action_37
action_106 (38) = happyShift action_38
action_106 (63) = happyShift action_39
action_106 (15) = happyGoto action_111
action_106 (16) = happyGoto action_32
action_106 (17) = happyGoto action_33
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_27

action_108 _ = happyReduce_29

action_109 (40) = happyShift action_43
action_109 (47) = happyShift action_44
action_109 (58) = happyShift action_45
action_109 (59) = happyShift action_46
action_109 (60) = happyShift action_47
action_109 (61) = happyShift action_48
action_109 (62) = happyShift action_49
action_109 (63) = happyShift action_39
action_109 (17) = happyGoto action_41
action_109 (20) = happyGoto action_110
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (43) = happyShift action_116
action_110 (46) = happyShift action_63
action_110 (47) = happyShift action_64
action_110 (48) = happyShift action_65
action_110 (49) = happyShift action_66
action_110 (50) = happyShift action_67
action_110 (51) = happyShift action_68
action_110 (52) = happyShift action_69
action_110 (53) = happyShift action_70
action_110 (54) = happyShift action_71
action_110 (55) = happyShift action_72
action_110 (56) = happyShift action_73
action_110 (57) = happyShift action_74
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (27) = happyShift action_115
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_26

action_113 _ = happyReduce_36

action_114 _ = happyReduce_20

action_115 _ = happyReduce_28

action_116 _ = happyReduce_32

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (GoatProgram happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 9 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENT happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Proc happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyTerminal (IDENT happy_var_3))
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Param happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn10
		 (Val
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn10
		 (Ref
	)

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn11
		 (BoolType
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (FloatType
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn11
		 (IntType
	)

happyReduce_15 = happySpecReduce_0  12 happyReduction_15
happyReduction_15  =  HappyAbsSyn12
		 ([]
	)

happyReduce_16 = happySpecReduce_2  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1:happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 13 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	(HappyTerminal (IDENT happy_var_2)) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Decl happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_0  14 happyReduction_18
happyReduction_18  =  HappyAbsSyn14
		 (Dim0
	)

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 _
	(HappyTerminal (INTEGRAL_CONST happy_var_2))
	_
	 =  HappyAbsSyn14
		 (Dim1 happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyTerminal (INTEGRAL_CONST happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (INTEGRAL_CONST happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Dim2 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  15 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1:happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Asg happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  16 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Read happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Write happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 6 16 happyReduction_26
happyReduction_26 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENT happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Call happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 16 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (If happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 7 16 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (IfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 5 16 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyTerminal (IDENT happy_var_1))
	 =  HappyAbsSyn17
		 (Var0 happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 17 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENT happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Var1 happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 7 17 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (IDENT happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Var2 happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  18 happyReduction_33
happyReduction_33  =  HappyAbsSyn18
		 ([]
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1:happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 (VarExpr happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyTerminal (BOOL_CONST happy_var_1))
	 =  HappyAbsSyn20
		 (BoolConst happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal (FRACTIONAL_CONST happy_var_1))
	 =  HappyAbsSyn20
		 (FloatConst happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyTerminal (INTEGRAL_CONST happy_var_1))
	 =  HappyAbsSyn20
		 (IntConst happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyTerminal (STR happy_var_1))
	 =  HappyAbsSyn20
		 (StrConst happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  20 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  20 happyReduction_43
happyReduction_43 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Add happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  20 happyReduction_44
happyReduction_44 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Sub happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Mul happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  20 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Div happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  20 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Equ happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  20 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr NEq happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  20 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr LTh happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  20 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr LEq happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  20 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr GTh happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  20 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr GEq happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  20 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr And happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  20 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (BinExpr Or happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  20 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (UnExpr Not happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  20 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (UnExpr Neg happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	BEGIN -> cont 21;
	BOOL -> cont 22;
	CALL -> cont 23;
	DO -> cont 24;
	ELSE -> cont 25;
	END -> cont 26;
	FI -> cont 27;
	FLOAT -> cont 28;
	IF -> cont 29;
	INT -> cont 30;
	OD -> cont 31;
	PROC -> cont 32;
	READ -> cont 33;
	REF -> cont 34;
	THEN -> cont 35;
	VAL -> cont 36;
	WHILE -> cont 37;
	WRITE -> cont 38;
	ASSIGN -> cont 39;
	LPAREN -> cont 40;
	RPAREN -> cont 41;
	LBRACKET -> cont 42;
	RBRACKET -> cont 43;
	COMMA -> cont 44;
	SEMI -> cont 45;
	ADD -> cont 46;
	SUB -> cont 47;
	MUL -> cont 48;
	DIV -> cont 49;
	EQU -> cont 50;
	NEQ -> cont 51;
	LTH -> cont 52;
	LTE -> cont 53;
	GTH -> cont 54;
	GTE -> cont 55;
	AND -> cont 56;
	OR -> cont 57;
	NOT -> cont 58;
	BOOL_CONST happy_dollar_dollar -> cont 59;
	FRACTIONAL_CONST happy_dollar_dollar -> cont 60;
	INTEGRAL_CONST happy_dollar_dollar -> cont 61;
	STR happy_dollar_dollar -> cont 62;
	IDENT happy_dollar_dollar -> cont 63;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 64 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
p tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parse :: String -> GoatProgram
parse s
  = p $ lexer s

parseError :: [Token] -> a
parseError tks
  = error $ "Parse error at token " ++
      spaced_list (map show (take 12 tks)) ++ " ..."

spaced_list :: [String] -> String
spaced_list
  = concat . intersperse " "

-- main
--  = do
--      s <- getContents
--      print (parse s)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/Library/Frameworks/GHC.framework/Versions/8.0.2-x86_64/usr/lib/ghc-8.0.2/include/ghcversion.h" #-}


















{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "/var/folders/s4/kh_rsdwd6dn782qn4phdvdr80000gn/T/ghc5864_0/ghc_2.h" #-}











































































































































































































































































































































































































































































































































































































































































































































































































































{-# LINE 18 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 










{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 137 "templates/GenericTemplate.hs" #-}


{-# LINE 147 "templates/GenericTemplate.hs" #-}
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
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

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

