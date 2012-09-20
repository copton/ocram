{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Representation where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Language.C.Data.Ident (internalIdent)
import Language.C.Data.Node (NodeInfo, undefNode)
import Ocram.Symbols (symbol, Symbol)

import qualified Compiler.Hoopl as H

data Variable -- {{{1
  -- |Local variables of functions, i.e. function parameters and automatic variables
  = Variable {
    var_decl   :: CDecl          -- ^the T-code declaration
  , var_unique :: Symbol         -- ^the unique variable name
  , var_scope  :: Maybe NodeInfo -- ^the node info of the surrounding T-code scope
  }

instance Eq Variable where
  v1 == v2 = var_unique v1 == var_unique v2

instance Ord Variable where
  compare v1 v2 = compare (var_unique v1) (var_unique v2)

instance Show Variable where
  show = show . var_unique

data Function -- {{{1
  -- |A critical function
  = Function {
    fun_cVars  :: [Variable] -- ^the function's critical variables
  , fun_ncVars :: [Variable] -- ^the function's non-critical variables
  , fun_stVars :: [Variable] -- ^the function's static variables
  , fun_def    :: CFunDef    -- ^the original AST node
  , fun_body   :: Body       -- ^the function's body as graph of basic blocks
  , fun_entry  :: Label      -- ^the entry point to the function
  }

fun_name :: Function -> Symbol -- {{{2
fun_name = symbol . fun_def

data Label -- {{{1
  -- |A label in the intermediate representation
  = TLabel Symbol H.Label -- ^A label originating from a T-code label
  | ILabel H.Label        -- ^A label required by the IR

instance Show Label where -- {{{2
  show (TLabel tl hl) = show hl ++ "/" ++ tl
  show (ILabel l)   = show l

hLabel :: Label -> H.Label -- {{{2
hLabel (TLabel _ l) = l
hLabel (ILabel l)   = l

data CriticalCall -- {{{1
  = FirstNormalForm Symbol [CExpr]
  | SecondNormalForm CExpr CAssignOp Symbol [CExpr]

instance Show CriticalCall where -- {{{2
  show (FirstNormalForm callee params) =
    (show . pretty) (CCall (CVar (internalIdent callee) undefNode) params undefNode)

  show (SecondNormalForm lhs op callee params) =
    (show . pretty) (CAssign op lhs (CCall (CVar (internalIdent callee) undefNode) params undefNode) undefNode)

data Node e x where -- {{{1
  -- |Constitutes of basic blocks
  Label  :: Label                   -> Node C O  -- ^'lbl: ;'. Entry point to a basic block
  Stmt   :: CExpr                   -> Node O O  -- ^any expression. The only middle parts of basic blocks
  Goto   :: Label                   -> Node O C  -- ^'goto label;'
  If     :: CExpr -> Label -> Label -> Node O C  -- ^'if (cond) {goto label1;} else {goto label2;}'
  Call   :: CriticalCall -> Label   -> Node O C  -- ^a critical call in normal form. The label belongs to the subsequent basic block.
  Return :: Maybe CExpr             -> Node O C  -- ^'return;' or 'return expr;'

instance H.NonLocal Node where -- {{{2
  entryLabel (Label l)    = hLabel l
  successors (Goto l)     = [hLabel l]
  successors (If _ tl el) = map hLabel [tl, el]
  successors (Call _ l)   = [hLabel l]
  successors (Return _)   = [] 

instance Show (Node e x) where -- {{{2
  showsPrec _ (Label l) =
      shows l
    . showString ":"
    . showChar '\n'

  showsPrec _ (Stmt expr) =
      (shows . pretty) expr
    . showChar ';'

  showsPrec _ (Goto l) =
      showString "GOTO "
    . shows l
    . showChar '\n'

  showsPrec _ (If cond tl el) =
      showString "IF "
    . (shows . pretty) cond
    . showString " THEN "
    . shows tl
    . showString " ELSE "
    . shows el
    . showChar '\n'

  showsPrec _ (Call call l) =
      shows call
    . showString "; GOTO "
    . shows l
    . showChar '\n'

  showsPrec _ (Return Nothing) = 
      showString "RETURN"
    . showChar '\n'

  showsPrec _ (Return (Just expr)) =
      showString "RETURN "
    . (shows . pretty) expr
    . showChar '\n'

type Body = H.Graph Node C C -- {{{1

type M = H.CheckingFuelMonad H.SimpleUniqueMonad -- {{{1

runIr :: M a -> a
runIr m = H.runSimpleUniqueMonad $ H.runWithFuel H.infiniteFuel m
