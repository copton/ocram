{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Representation where

-- imports {{{1
import Compiler.Hoopl (C, O, Graph, NonLocal(..), Label)
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Ocram.Symbols (symbol, Symbol)

type FQN -- {{{1
  -- |A fully qualified name
  = String

data Variable -- {{{1
  -- |Local variables of functions, i.e. function parameters and automatic variables
  = Variable {
    var_decl  :: CDecl    -- ^the T-code declaration
  , var_fqn   :: FQN      -- ^the unique variable name
  , var_scope :: NodeInfo -- ^the node info of the surrounding scope
  }

data Function -- {{{1
  -- |A critical function
  = Function {
    fun_vars  :: [Variable]       -- ^the function's variables
  , fun_def   :: CFunDef          -- ^the original AST node
  , fun_body  :: Graph Node C C -- ^the function's body as graph of basic blocks
  }

fun_name :: Function -> Symbol -- {{{2
fun_name = symbol . fun_def

data Node e x where -- {{{1
  -- |Constitutes of basic blocks
  Label  :: Label                   -> Node C O  -- ^'lbl: ;'. Entry point to a basic block
  Stmt   :: CExpr                   -> Node O O  -- ^any expression. The only middle parts of basic blocks
  Goto   :: Label                   -> Node O C  -- ^'goto label;'
  If     :: CExpr -> Label -> Label -> Node O C  -- ^'if (cond) {goto label1;} else {goto label2;}'
  Call   :: CExpr -> Label          -> Node O C  -- ^a critical call in normal form. The label belongs to the subsequent basic block.
  Return :: Maybe CExpr             -> Node O C  -- ^'return;' or 'return expr;'

instance NonLocal Node where -- {{{2
  entryLabel (Label l)    = l
  successors (Goto l)     = [l]
  successors (If _ tl el) = [tl, el]
  successors (Call _ l)   = [l]
  successors (Return _)   = [] 
