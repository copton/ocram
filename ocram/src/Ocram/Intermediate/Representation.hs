{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Representation where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Language.C.Syntax.AST (CAssignOp, CExpression(CCall, CAssign, CVar), CDecl, Annotated(annotation), CFunDef)
import Language.C.Data.Ident (internalIdent)
import Ocram.Debug.Enriched (CExpr', eun, ENodeInfo(..)) -- avoid dependency cycle
import Ocram.Print (render)
import Ocram.Ruab (TRow)
import Ocram.Symbols (Symbol, CSymbol(symbol))

import qualified Compiler.Hoopl as H

data Variable -- {{{1
  -- Variables declared within a function
  = TVariable {
  -- |Variables declared in the T-code
    var_tname  :: Symbol       -- ^the original name of the varialbe
  , var_decl   :: CDecl        -- ^the declaration using a unique variable name
  , var_scope  :: (TRow, TRow) -- ^the first and last row of the surrounding T-code scope
  }
  | EVariable {
  -- |Variable originating from transformation
    var_decl   :: CDecl        -- ^the declaration using a unique variable name
  }

instance CSymbol Variable where -- {{{2
  symbol = symbol . var_decl

instance Eq Variable where -- {{{2
  v1 == v2 = symbol v1 == symbol v2

instance Ord Variable where -- {{{2
  compare v1 v2 = compare (symbol v1) (symbol v2)

instance Show Variable where -- {{{2
  show = show . symbol

data FunctionVariable = FunctionVariable { -- {{{1
    fvar_var       :: Variable
  , fvar_storage   :: VariableStorage
  , fvar_parameter :: Bool
  , fvar_critical  :: Bool
  } deriving (Show)

fvarParameter :: Variable -> FunctionVariable
fvarParameter var = FunctionVariable var VarAutomatic True True

fvarAuto :: Variable -> FunctionVariable
fvarAuto var = FunctionVariable var VarAutomatic False True

fvarStatic :: Variable -> FunctionVariable
fvarStatic var = FunctionVariable var VarStatic False True

fvar_static :: FunctionVariable -> Bool
fvar_static fvar = case fvar_storage fvar of
  VarStatic -> True
  _         -> False

fvar_auto :: FunctionVariable -> Bool
fvar_auto fvar = case fvar_storage fvar of
  VarAutomatic -> True
  _            -> False

data VariableStorage -- {{{2
  = VarAutomatic
  | VarStatic
  deriving Show

instance CSymbol FunctionVariable where
  symbol = symbol . fvar_var

type VariableCritical = Bool -- {{{2

type VariableParameter = Bool -- {{{2

data Function -- {{{1
  -- |A critical function
  = Function {
    fun_vars  :: [FunctionVariable] -- ^the variables declared in the function
  , fun_def   :: CFunDef            -- ^the original AST node
  , fun_body  :: Body               -- ^the function's body as graph of basic blocks
  , fun_entry :: Label              -- ^the entry point to the function
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
  = FirstNormalForm Symbol [CExpr'] ENodeInfo
  | SecondNormalForm CExpr' CAssignOp Symbol [CExpr'] ENodeInfo

instance Show CriticalCall where -- {{{2
  show (FirstNormalForm callee params _) =
    render (CCall (CVar (internalIdent callee) eun) params eun)

  show (SecondNormalForm lhs op callee params _) =
    render (CAssign op lhs (CCall (CVar (internalIdent callee) eun) params eun) eun)

data Node e x where -- {{{1
  -- |Constitutes of basic blocks
  Label  :: Label                                 -> Node C O  -- ^'lbl: ;'. Entry point to a basic block
  Cont   :: Label -> CriticalCall                 -> Node C O  -- ^continuation of a critical call in second normal form
  Stmt   :: CExpr'                                -> Node O O  -- ^any expression. The only middle parts of basic blocks
  Goto   :: Label                                 -> Node O C  -- ^'goto label;'
  If     :: CExpr' -> Label -> Label -> ENodeInfo -> Node O C  -- ^'if (cond) {goto label1;} else {goto label2;}'
  Call   :: CriticalCall -> Label                 -> Node O C  -- ^a critical call in normal form. The label belongs to the subsequent basic block.
  Return :: Maybe CExpr' -> ENodeInfo             -> Node O C  -- ^'return;' or 'return expr;'

instance H.NonLocal Node where -- {{{2
  entryLabel (Label l)      = hLabel l
  entryLabel (Cont l _)     = hLabel l
  successors (Goto l)       = [hLabel l]
  successors (If _ tl el _) = map hLabel [tl, el]
  successors (Call _ l)     = [hLabel l]
  successors (Return _ _)   = [] 

instance Show (Node e x) where -- {{{2
  showsPrec _ (Label l) =
      shows l
    . showString ":"
    . showChar '\n'

  showsPrec _ (Cont l call) =
      shows l
    . showString ": "
    . shows call
    . showChar '\n'

  showsPrec _ (Stmt expr) =
      (showString . render) expr
    . showChar ';'

  showsPrec _ (Goto l) =
      showString "GOTO "
    . shows l
    . showChar '\n'

  showsPrec _ (If cond tl el _) =
      showString "IF "
    . (showString . render) cond
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

  showsPrec _ (Return Nothing _) = 
      showString "RETURN"
    . showChar '\n'

  showsPrec _ (Return (Just expr) _) =
      showString "RETURN "
    . (showString . render) expr
    . showChar '\n'

get_enode_info :: Node e x -> ENodeInfo -- {{{1
-- XXX how to make an Annotated instance of Node and CriticalCall?
get_enode_info (Label _)    = EnUndefined
get_enode_info (Cont _ c)   = get_enode_info' c
get_enode_info (Stmt e)     = annotation e
get_enode_info (Goto _)     = EnUndefined
get_enode_info (If _ _ _ n) = n
get_enode_info (Call c _)   = get_enode_info' c
get_enode_info (Return _ n) = n

get_enode_info' :: CriticalCall -> ENodeInfo
get_enode_info' (FirstNormalForm _ _ n) = n
get_enode_info' (SecondNormalForm _ _ _ _ n) = n

type Body = H.Graph Node C C -- {{{1

type Block = H.Block Node C C -- {{{1

type BlockMap = H.LabelMap Block -- {{{1

type M = H.CheckingFuelMonad H.SimpleUniqueMonad -- {{{1

runIr :: M a -> a -- {{{2
runIr m = H.runSimpleUniqueMonad $ H.runWithFuel H.infiniteFuel m

block_components :: Block -> (Node C O, [Node O O], Node O C) -- {{{1
block_components block = let ([first], middles, [last']) = H.foldBlockNodesF3 (ffirst, fmiddle, flast) block ([], [], []) in (first, reverse middles, last')
  where
    ffirst  x (a, b, c) = (x:a, b, c)
    fmiddle x (a, b, c) = (a, x:b, c)
    flast   x (a, b, c) = (a, b, x:c)

block_map :: Body -> BlockMap -- {{{1
block_map (H.GMany H.NothingO blocks H.NothingO) = blocks

form_body :: BlockMap -> Body -- {{{1
form_body blocks = H.GMany H.NothingO blocks H.NothingO
