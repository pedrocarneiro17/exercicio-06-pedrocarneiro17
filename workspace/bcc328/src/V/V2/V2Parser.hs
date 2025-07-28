{-# OPTIONS_GHC -w #-}
module V.V2.V2Parser where

import Utils.Value
import Utils.Var
import V.V2.V2Lexer hiding (lexer)
import V.V2.Instr
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Code)
	| HappyAbsSyn5 (Instr)
	| HappyAbsSyn6 (Value)
	| HappyAbsSyn7 (Var)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
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
 action_35 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

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
 happyReduce_29 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,64) ([64512,64831,49159,54271,127,16380,2045,40,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Code","Instr","Val","Var","num","ident","string","'push'","'add'","'minus'","'mul'","'div'","'lt'","'cat'","'size'","'eq'","'input'","'load'","'store'","'true'","'false'","'print'","';'","'and'","'not'","'i2s'","'i2b'","'s2i'","'s2b'","'b2s'","'b2i'","'halt'","%eof"]
        bit_start = st Prelude.* 36
        bit_end = (st Prelude.+ 1) Prelude.* 36
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..35]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11) = happyShift action_3
action_0 (12) = happyShift action_4
action_0 (13) = happyShift action_5
action_0 (14) = happyShift action_6
action_0 (15) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (17) = happyShift action_9
action_0 (18) = happyShift action_10
action_0 (19) = happyShift action_11
action_0 (20) = happyShift action_12
action_0 (21) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (25) = happyShift action_15
action_0 (27) = happyShift action_16
action_0 (28) = happyShift action_17
action_0 (29) = happyShift action_18
action_0 (30) = happyShift action_19
action_0 (31) = happyShift action_20
action_0 (32) = happyShift action_21
action_0 (33) = happyShift action_22
action_0 (34) = happyShift action_23
action_0 (35) = happyShift action_24
action_0 (4) = happyGoto action_25
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (11) = happyShift action_3
action_1 (12) = happyShift action_4
action_1 (13) = happyShift action_5
action_1 (14) = happyShift action_6
action_1 (15) = happyShift action_7
action_1 (16) = happyShift action_8
action_1 (17) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (19) = happyShift action_11
action_1 (20) = happyShift action_12
action_1 (21) = happyShift action_13
action_1 (22) = happyShift action_14
action_1 (25) = happyShift action_15
action_1 (27) = happyShift action_16
action_1 (28) = happyShift action_17
action_1 (29) = happyShift action_18
action_1 (30) = happyShift action_19
action_1 (31) = happyShift action_20
action_1 (32) = happyShift action_21
action_1 (33) = happyShift action_22
action_1 (34) = happyShift action_23
action_1 (35) = happyShift action_24
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (11) = happyShift action_3
action_2 (12) = happyShift action_4
action_2 (13) = happyShift action_5
action_2 (14) = happyShift action_6
action_2 (15) = happyShift action_7
action_2 (16) = happyShift action_8
action_2 (17) = happyShift action_9
action_2 (18) = happyShift action_10
action_2 (19) = happyShift action_11
action_2 (20) = happyShift action_12
action_2 (21) = happyShift action_13
action_2 (22) = happyShift action_14
action_2 (25) = happyShift action_15
action_2 (27) = happyShift action_16
action_2 (28) = happyShift action_17
action_2 (29) = happyShift action_18
action_2 (30) = happyShift action_19
action_2 (31) = happyShift action_20
action_2 (32) = happyShift action_21
action_2 (33) = happyShift action_22
action_2 (34) = happyShift action_23
action_2 (35) = happyShift action_24
action_2 (4) = happyGoto action_34
action_2 (5) = happyGoto action_2
action_2 _ = happyReduce_2

action_3 (8) = happyShift action_30
action_3 (10) = happyShift action_31
action_3 (23) = happyShift action_32
action_3 (24) = happyShift action_33
action_3 (6) = happyGoto action_29
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 _ = happyReduce_8

action_9 _ = happyReduce_12

action_10 _ = happyReduce_13

action_11 _ = happyReduce_9

action_12 _ = happyReduce_20

action_13 (9) = happyShift action_27
action_13 (7) = happyGoto action_28
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_27
action_14 (7) = happyGoto action_26
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_21

action_16 _ = happyReduce_10

action_17 _ = happyReduce_11

action_18 _ = happyReduce_14

action_19 _ = happyReduce_15

action_20 _ = happyReduce_18

action_21 _ = happyReduce_19

action_22 _ = happyReduce_16

action_23 _ = happyReduce_17

action_24 _ = happyReduce_24

action_25 (36) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_23

action_27 _ = happyReduce_29

action_28 _ = happyReduce_22

action_29 (26) = happyShift action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_25

action_31 _ = happyReduce_26

action_32 _ = happyReduce_27

action_33 _ = happyReduce_28

action_34 _ = happyReduce_1

action_35 _ = happyReduce_3

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Push happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 (Add
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 (Sub
	)

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn5
		 (Mul
	)

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 (Div
	)

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn5
		 (Lt
	)

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (IEq
	)

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (And
	)

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn5
		 (Not
	)

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn5
		 (Cat
	)

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (Size
	)

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn5
		 (I2S
	)

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (I2B
	)

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (B2S
	)

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (B2I
	)

happyReduce_18 = happySpecReduce_1  5 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn5
		 (S2I
	)

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn5
		 (S2B
	)

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn5
		 (Input
	)

happyReduce_21 = happySpecReduce_1  5 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn5
		 (Print
	)

happyReduce_22 = happySpecReduce_2  5 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Load happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  5 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Store happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  5 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn5
		 (Halt
	)

happyReduce_25 = happySpecReduce_1  6 happyReduction_25
happyReduction_25 (HappyTerminal (Token (TNum happy_var_1) _))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  6 happyReduction_26
happyReduction_26 (HappyTerminal (Token (TString happy_var_1) _))
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  6 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn6
		 (VBool True
	)

happyReduce_28 = happySpecReduce_1  6 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn6
		 (VBool False
	)

happyReduce_29 = happySpecReduce_1  7 happyReduction_29
happyReduction_29 (HappyTerminal (Token (TIdentifier happy_var_1) _))
	 =  HappyAbsSyn7
		 (Var happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token TEOF _ -> action 36 36 tk (HappyState action) sts stk;
	Token (TNum happy_dollar_dollar) _ -> cont 8;
	Token (TIdentifier happy_dollar_dollar) _ -> cont 9;
	Token (TString happy_dollar_dollar) _ -> cont 10;
	Token TPush _ -> cont 11;
	Token TAdd _ -> cont 12;
	Token TMinus _ -> cont 13;
	Token TMul _ -> cont 14;
	Token TDiv _ -> cont 15;
	Token TLt _ -> cont 16;
	Token TCat _ -> cont 17;
	Token TSize _ -> cont 18;
	Token TEq _ -> cont 19;
	Token TInput _ -> cont 20;
	Token TLoad _ -> cont 21;
	Token TStore _ -> cont 22;
	Token TTrue _ -> cont 23;
	Token TFalse _ -> cont 24;
	Token TPrint _ -> cont 25;
	Token TSemi _ -> cont 26;
	Token TAnd _ -> cont 27;
	Token TNot _ -> cont 28;
	Token TI2S _ -> cont 29;
	Token TI2B _ -> cont 30;
	Token TS2I _ -> cont 31;
	Token TS2B _ -> cont 32;
	Token TB2S _ -> cont 33;
	Token TB2I _ -> cont 34;
	Token THalt _ -> cont 35;
	_ -> happyError' (tk, [])
	})

happyError_ explist 36 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parser = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parserTest :: String -> IO ()
parserTest s = do
  r <- codeParser s
  print r

parseError :: Token -> Alex a
parseError (Token lexeme (line, col)) =
  alexError $ "Parse error while processing lexeme: " ++ show lexeme
              ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

codeParser :: String -> IO (Either String Code)
codeParser content = do
  pure $ runAlex content parser
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

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
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
