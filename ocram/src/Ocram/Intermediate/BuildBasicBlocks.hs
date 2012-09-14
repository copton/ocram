{-# LANGUAGE TemplateHaskell #-}
module Ocram.Intermediate.BuildBasicBlocks
-- exports {{{1
(
  build_basic_blocks
) where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Data.Maybe (isNothing)
import Ocram.Symbols (Symbol, symbol)
import Language.C.Syntax.AST
import Ocram.Util (fromJust_s, head_s, abort, unexp)

import qualified Ocram.Intermediate.Representation as I
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

build_basic_blocks :: S.Set Symbol -> [CStat] -> I.Body -- {{{1
build_basic_blocks cf stmts = runM $ do
  protoblocks <- partition cf stmts
  let blockcont = zip protoblocks $ map pbLabel (tail protoblocks) ++ [undefined]
  blocks <- mapM convert blockcont
  return $ foldl splice H.emptyClosedGraph blocks
  where splice = (H.|*><*|)

partition :: S.Set Symbol -> [CStat] -> M [ProtoBlock] -- {{{2
partition cf stmts =
  let annotatedStmts = zip (map splitPoint stmts) stmts in
  part annotatedStmts

  where
    part []     = return []
    part astmts = case head astmts of
      (_, CLabel name _ _ _) -> do
        ilabel <- labelFor (symbol name)
        buildBlock ilabel (tail astmts)
      _                 -> do
        ilabel <- newILabel
        buildBlock ilabel astmts
      
    buildBlock ilabel astmts =  -- {{{3
      let
        (prefix, suffix) = span (isNothing . fst) astmts
        (block, rest) = case ($fromJust_s . fst . $head_s) suffix of
          SplitBefore -> (prefix, suffix)
          SplitAfter  -> (prefix ++ [head suffix], tail suffix)
      in
        part rest >>= return . (ProtoBlock ilabel block :)

    splitPoint :: CStat -> Maybe SplitPoint -- {{{3
    splitPoint (CLabel _ _ _ _)      = Just SplitBefore
    splitPoint (CIf _ _ _ _)         = Just SplitAfter
    splitPoint (CGoto _ _ )          = Just SplitAfter
    splitPoint (CExpr (Just expr) _) =
      case getCallee expr of
        Nothing -> Nothing
        Just callee ->
          if S.member (symbol callee) cf
            then Just SplitAfter
            else Nothing
    splitPoint (CReturn _ _)        = Just SplitAfter
    splitPoint o                    = $abort $ unexp o

    getCallee (CCall (CVar callee _) _ _)                 = Just callee -- {{{3
    getCallee (CAssign _ _ (CCall (CVar callee _) _ _) _) = Just callee
    getCallee _                                           = Nothing

convert :: (ProtoBlock, I.Label) -> M I.Body -- {{{2
convert ((ProtoBlock thisBlock body), nextBlock) = do
  let nfirst = I.Label thisBlock
  nmiddles <- mapM convM (init body)
  (lastMiddle, nlast) <- convL (last body)
  let
    nmiddles' = case lastMiddle of
      Nothing -> nmiddles
      Just x  -> nmiddles ++ [x]
  return $ H.mkFirst nfirst `splice` H.mkMiddles nmiddles' `splice` H.mkLast nlast
  where
    splice = (H.<*>)
 
    convM :: AnnotatedStmt -> M (I.Node O O)
    convM (Nothing, CExpr (Just e) _) = return $ I.Stmt e
    convM (x, y)                      = $abort $ unexp y ++ ", " ++ show x

    convL :: AnnotatedStmt -> M (Maybe (I.Node O O), I.Node O C)
    convL (Just SplitAfter, CIf cond (CGoto target _) Nothing _) = do
      itarget <- labelFor (symbol target)
      return (Nothing, I.If cond itarget nextBlock)

    convL (Just SplitAfter, CIf cond (CGoto ttarget _) (Just (CGoto etarget _)) _) = do
      ittarget <- labelFor (symbol ttarget)
      ietarget <- labelFor (symbol etarget)
      return (Nothing, I.If cond ittarget ietarget)

    convL (Nothing, CExpr (Just expr) _) =
      return (Just (I.Stmt expr), I.Goto nextBlock)

    convL (Just SplitAfter, CExpr (Just expr) _) =
      return (Nothing, I.Call expr nextBlock)

    convL (Just SplitAfter, CReturn expr _) =
      return (Nothing, I.Return expr)

    convL (Just SplitAfter, CGoto target _) = do
      itarget <- labelFor (symbol target)
      return (Nothing, I.Goto itarget)

    convL (x, y) = $abort $ unexp y ++ ", " ++ show x
      
-- types {{{1
type AnnotatedStmt = (Maybe SplitPoint, CStat)

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
