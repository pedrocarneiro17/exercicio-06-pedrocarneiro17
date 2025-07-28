{-# OPTIONS_GHC -w #-}
{-# OPTIONS -cpp #-}
module L.L2.Frontend.HappyParser (runL2Parser, printL2Tree) where
import L.L2.Frontend.Lexer
import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,83) ([3200,32,32818,0,4096,2048,0,16,0,2,2048,8192,0,0,0,0,6,6144,0,0,16,3584,2,32,57344,32,3200,32,0,0,480,0,0,0,0,0,2104,0,16,0,31,526,0,4096,0,0,0,0,376,0,57344,32,33664,0,526,14336,8,32,0,7936,0,0,4096,0,0,0,0,24576,0,384,800,8,32768,0,0,200,2,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgram","Program","StmtList","Stmt","Expr","TIdent","TNumber","TString","TRead","TPrint","TAssign","TSemicolon","TComma","TLParen","TRParen","TPlus","TMinus","TTimes","TDiv","TDef","TIn","TEnd","TEOF","%eof"]
        bit_start = st Prelude.* 26
        bit_end = (st Prelude.+ 1) Prelude.* 26
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..25]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyActOffsets = Happy_Data_Array.listArray (0,48) ([1,1,-17,10,8,13,41,48,32,-14,20,0,47,6,49,6,1,0,-1,0,0,0,6,50,14,6,36,0,0,7,0,6,6,6,6,54,19,0,51,0,0,26,26,1,39,0,1,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyGotoOffsets = Happy_Data_Array.listArray (0,48) ([34,40,0,0,0,0,0,0,0,0,0,0,0,56,0,57,42,0,0,0,0,0,59,0,0,60,0,0,0,0,0,61,62,63,64,0,0,0,0,0,0,0,0,44,0,0,46,0,0
	])

happyAdjustOffset :: Prelude.Int -> Prelude.Int
happyAdjustOffset = Prelude.id

happyDefActions :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyDefActions = Happy_Data_Array.listArray (0,48) ([-5,0,0,0,0,0,0,0,0,0,0,-2,0,0,0,0,-5,-6,-8,-12,-11,-13,0,0,0,0,0,-3,-4,0,-10,0,0,0,0,0,0,-18,0,-17,-16,-15,-14,-5,0,-9,-5,-7
	])

happyCheck :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyCheck = Happy_Data_Array.listArray (0,83) ([-1,18,1,17,18,4,5,1,2,3,11,12,13,14,6,9,15,7,11,12,13,14,9,16,10,11,12,13,14,10,11,12,13,14,0,1,2,17,18,13,14,1,2,1,2,1,2,1,2,1,9,19,3,6,18,1,17,-1,8,3,3,10,3,3,3,3,3,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyTable = Happy_Data_Array.listArray (0,83) ([0,12,5,11,12,6,7,20,21,22,32,33,34,35,16,23,8,17,32,33,34,35,15,44,31,32,33,34,35,38,32,33,34,35,8,9,3,27,28,34,35,2,3,17,3,44,3,47,3,13,14,-1,24,26,29,39,47,0,36,24,18,46,36,29,42,41,40,39,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 17) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17)
	]

happy_n_terms = 20 :: Prelude.Int
happy_n_nonterms = 4 :: Prelude.Int

happyReduce_1 = happySpecReduce_2  0 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  0 happyReduction_2
happyReduction_2 _
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 0 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_1
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_0  1 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 ([]
	)

happyReduce_5 = happySpecReduce_3  1 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 8 1 happyReduction_6
happyReduction_6 ((HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Def (Var happy_var_2) happy_var_4 happy_var_6 : happy_var_8
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  2 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn6
		 (LAssign (Var happy_var_1) happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 2 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TString happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (LRead happy_var_3 (Var happy_var_5)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 2 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (LPrint happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  3 happyReduction_10
happyReduction_10 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn7
		 (LVal (VInt happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  3 happyReduction_11
happyReduction_11 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn7
		 (LVar (Var happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  3 happyReduction_12
happyReduction_12 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn7
		 (LVal (VStr happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  3 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LAdd happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  3 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LMinus happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  3 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LMul happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  3 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LDiv happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  3 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 19 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	Token _ (TIdent happy_dollar_dollar) -> cont 1;
	Token _ (TNumber happy_dollar_dollar) -> cont 2;
	Token _ (TString happy_dollar_dollar) -> cont 3;
	Token _ TRead -> cont 4;
	Token _ TPrint -> cont 5;
	Token _ TAssign -> cont 6;
	Token _ TSemicolon -> cont 7;
	Token _ TComma -> cont 8;
	Token _ TLParen -> cont 9;
	Token _ TRParen -> cont 10;
	Token _ TPlus -> cont 11;
	Token _ TMinus -> cont 12;
	Token _ TTimes -> cont 13;
	Token _ TDiv -> cont 14;
	Token _ TDef -> cont 15;
	Token _ TIn -> cont 16;
	Token _ TEnd -> cont 17;
	Token _ TEOF -> cont 18;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 19 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseProgram tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error "Erro de análise sintática: código vazio!"
parseError ts@(t:_) = error $ "Erro de análise sintática na posição " ++ show (pos t) ++ "! Tokens restantes: " ++ show (map lexeme ts)

runL2Parser :: [Token] -> [S2]
runL2Parser = parseProgram

-- Função auxiliar para criar a indentação
indent :: Int -> String
indent n = replicate (n * 2) ' '

-- Função principal que inicia a impressão da árvore
printL2Tree :: [S2] -> String
printL2Tree stmts = "L2\n" ++ concatMap (pprStmt 1) stmts

-- Função para imprimir um comando (S2) com um nível de indentação
pprStmt :: Int -> S2 -> String
pprStmt level s = case s of
  Def var expr block ->
    indent level ++ "Def\n" ++
    pprVar (level + 1) var ++
    pprExpr (level + 1) expr ++
    concatMap (pprStmt (level + 1)) block
  LAssign var expr ->
    indent level ++ "LAssign\n" ++
    pprVar (level + 1) var ++
    pprExpr (level + 1) expr
  LRead prompt var ->
    indent level ++ "LRead \"" ++ prompt ++ "\"\n" ++
    pprVar (level + 1) var
  LPrint expr ->
    indent level ++ "LPrint\n" ++
    pprExpr (level + 1) expr

-- Função para imprimir uma expressão (E2)
pprExpr :: Int -> E2 -> String
pprExpr level e = case e of
  LAdd e1 e2 -> indent level ++ "LAdd\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LMinus e1 e2 -> indent level ++ "LMinus\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LMul e1 e2 -> indent level ++ "LMul\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LDiv e1 e2 -> indent level ++ "LDiv\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LVar var -> indent level ++ "LVar\n" ++ pprVar (level + 1) var
  LVal val -> indent level ++ "LVal\n" ++ pprValue (level + 1) val

-- Função para imprimir uma variável (Var)
pprVar :: Int -> Var -> String
pprVar level (Var v) = indent level ++ "Var \"" ++ v ++ "\"\n"

-- Função para imprimir um valor (Value)
pprValue :: Int -> Value -> String
pprValue level v = case v of
  VInt n  -> indent level ++ "VInt " ++ show n ++ "\n"
  VStr s  -> indent level ++ "VStr \"" ++ s ++ "\"\n"
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
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                (0)           -> {- nothing -}
                                     happyFail (happyExpListPerState ((st) :: Prelude.Int)) i tk st
                (-1)          -> {- nothing -}
                                     happyAccept i tk st
                n | (n Prelude.< ((0) :: Prelude.Int)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = ((Prelude.negate ((n Prelude.+ ((1) :: Prelude.Int)))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Prelude.- ((1) :: Prelude.Int))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Prelude.+ i)
         check  = if (off_i Prelude.>= ((0) :: Prelude.Int))
                  then (indexShortOffAddr happyCheck off_i Prelude.== i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st












indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail [] (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Prelude.+ nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Prelude.+ nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (0) tk old_st _ stk@(x `HappyStk` _) =
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction (0) tk action sts ((HappyErrorToken (i)) `HappyStk` stk)

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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
