{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.BuildBasicBlocks
-- exports {{{1
(
  build_basic_blocks
) where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Data.Foldable (foldrM)
import Data.Maybe (isNothing, catMaybes)
import Language.C.Syntax.AST
import Ocram.Debug (CStat', ENodeInfo)
import Ocram.Symbols (Symbol, symbol)
import Ocram.Util (fromJust_s, head_s, abort, unexp)

import qualified Ocram.Intermediate.Representation as I
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

build_basic_blocks :: S.Set Symbol -> ENodeInfo -> [CStat'] -> (I.Label, I.Body) -- {{{1
build_basic_blocks cf endOfFunction stmts = runM $ do
  protoblocks <- partition cf endOfFunction . annotate cf . removeEmptyExpressions $ stmts
  let blockcont = zip protoblocks $ map pbLabel (tail protoblocks) ++ [undef]
  (blocks, _) <- foldrM convert ([], Nothing) (reverse blockcont)
  let body = foldl splice H.emptyClosedGraph blocks
  return ((pbLabel . $head_s) protoblocks, body)
  where
    splice = (H.|*><*|)
    undef = $abort "undefined"

removeEmptyExpressions :: [CStat'] -> [CStat']
removeEmptyExpressions = foldr rm []
  where
    rm (CExpr Nothing _) ss = ss
    rm o                 ss = o : ss

annotate :: S.Set Symbol -> [CStat'] -> [AnnotatedStmt] -- {{{2
annotate cf stmts = zip (map splitPoint stmts) stmts
  where
    splitPoint (CLabel _ _ _ _)         = Just SplitBefore
    splitPoint (CIf _ _ _ _)            = Just SplitAfter
    splitPoint (CGoto _ _ )             = Just SplitAfter
    splitPoint (CExpr (Just expr) _)    =
      case getCallee expr of
        Nothing                        -> Nothing
        Just callee ->
          if S.member (symbol callee) cf
                                     then Just SplitAfter
                                     else Nothing

    splitPoint (CReturn _ _)            = Just SplitAfter
    splitPoint o                        = $abort $ unexp o

    getCallee (CCall (CVar callee _) _ _)                 = Just callee
    getCallee (CAssign _ _ (CCall (CVar callee _) _ _) _) = Just callee
    getCallee _                                           = Nothing

partition :: S.Set Symbol -> ENodeInfo -> [AnnotatedStmt] -> M [ProtoBlock] -- {{{2
partition cf endOfFunction = part unused
  where
    unused = $abort "empty critical function?"
    explicitReturn = (Just SplitAfter, CReturn Nothing endOfFunction)

    sequel = do
      ilabel <- newILabel
      return $ [ProtoBlock ilabel [explicitReturn]]

    part previousStatement [] = case previousStatement of
      -- handling "fall through" at the end of the function
      (Just SplitAfter, expr) -> 
        case expr of
          CIf _ _ Nothing _   -> sequel
          CExpr _ _           -> sequel
          _                   -> return []
      (x, y)                  -> $abort $ unexp y ++ ", " ++ show x
      
    part _ astmts = case head astmts of
      (_, CLabel name _ _ _) -> do
        ilabel <- labelFor (symbol name)
        buildBlock ilabel (tail astmts)
      _                 -> do
        ilabel <- newILabel
        buildBlock ilabel astmts
      
    buildBlock ilabel astmts =  -- {{{3
      let
        (prefix, suffix) = span (isNothing . fst) astmts
        (block, rest) = case suffix of
          [] -> (prefix ++ [explicitReturn], []) -- handling implicit return
          (split:rest') -> case ($fromJust_s . fst) split of
            SplitBefore -> (prefix, suffix)
            SplitAfter  -> (prefix ++ [split], rest')
      in do
        subsequentBlocks <- part (last block) rest
        return $ ProtoBlock ilabel block : subsequentBlocks


convert :: (ProtoBlock, I.Label) -> ([I.Body], Maybe I.CriticalCall) -> M ([I.Body], Maybe I.CriticalCall) -- {{{2
convert (ProtoBlock thisBlock body, nextBlock) (blocks, mcall) 
  | null body = return (H.mkFirst entry `splice` H.mkLast (I.Goto nextBlock) : blocks, Nothing)
  | otherwise = do
      nmiddles                   <- mapM convM (init body)
      (lastMiddles, nlast, call) <- convL (last body)
      let block                  = H.mkFirst entry
                          `splice` H.mkMiddles (catMaybes nmiddles ++ lastMiddles)
                          `splice` H.mkLast nlast
      return (block : blocks, call)
  where
    splice = (H.<*>)

    entry = case mcall of
      Nothing   -> I.Label thisBlock
      Just call -> I.Cont thisBlock call
 
    convM :: AnnotatedStmt -> M (Maybe (I.Node O O))
    convM (Nothing, CExpr (Just e) _) = return $ Just $ I.Stmt e
    convM (x, y)                      = $abort $ unexp y ++ ", " ++ show x

    convL :: AnnotatedStmt -> M ([I.Node O O], I.Node O C, Maybe I.CriticalCall)
    convL (Just SplitAfter, CIf cond (CGoto target _) Nothing eni) = do
      itarget <- labelFor (symbol target)
      return ([], I.If cond itarget nextBlock eni, Nothing)

    convL (Just SplitAfter, CIf cond (CGoto ttarget _) (Just (CGoto etarget _)) eni) = do
      ittarget <- labelFor (symbol ttarget)
      ietarget <- labelFor (symbol etarget)
      return ([], I.If cond ittarget ietarget eni, Nothing)

    convL (Nothing, CExpr (Just expr) _) =
      return ([I.Stmt expr], I.Goto nextBlock, Nothing)

    convL (Just SplitAfter, CExpr (Just (CCall (CVar callee _) params eni)) _) =
      let call = I.FirstNormalForm (symbol callee) params eni in
      return ([], I.Call call nextBlock, Just call)

    convL (Just SplitAfter, CExpr (Just (CAssign op lhs (CCall (CVar callee _) params _) eni)) _) =
      let call = I.SecondNormalForm lhs op (symbol callee) params eni in
      return ([], I.Call call nextBlock, Just call)

    convL (Just SplitAfter, CReturn expr eni) =
      return ([] , I.Return expr eni, Nothing)

    convL (Just SplitAfter, CGoto target _) = do
      itarget <- labelFor (symbol target)
      return ([] , I.Goto itarget, Nothing)

    convL (x, y) = $abort $ unexp y ++ ", " ++ show x
      
-- types {{{1
type AnnotatedStmt = (Maybe SplitPoint, CStat') -- {{{2

data ProtoBlock -- {{{2
  -- |Prototype of a basic block
  = ProtoBlock {
      pbLabel :: I.Label         -- ^The entry label of the block
    , pbBody  :: [AnnotatedStmt] -- ^The body of the block
    }

data SplitPoint -- {{{2
  = SplitBefore
  | SplitAfter
  deriving Show


data M a -- {{{1
  -- |The monad
  = M (LabelMap -> I.M (LabelMap, a))

runM :: M a -> a
runM (M f) = (snd . I.runIr . f) M.empty

type LabelMap -- {{{2
  -- |Map from T-code label name to IR label
  = M.Map Symbol I.Label

instance Monad M where -- {{{2
  return x = M (\m -> return (m, x))

  M f1 >>= k =
    M (\m -> do
        (m', x) <- f1 m
        let (M f2) = k x
        f2 m'
      )

labelFor :: Symbol -> M I.Label -- {{{2
labelFor name = M f
  where
    f m = 
      case M.lookup name m of
        Just ilabel -> return (m, ilabel)
        Nothing     -> do
          hlabel  <- H.freshLabel
          let ilabel = I.TLabel name hlabel
          let m' = M.insert name ilabel m
          return (m', ilabel)

newILabel :: M I.Label -- {{{2
newILabel = M f
  where
    f m = do
      hlabel <- H.freshLabel
      return (m, I.ILabel hlabel)
