{-# LANGUAGE GADTs #-}
module Ocram.Intermediate.Representation where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Language.C.Syntax.AST
import Language.C.Pretty (pretty)
import Language.C.Data.Node (NodeInfo)
import Ocram.Symbols (symbol, Symbol)

import qualified Compiler.Hoopl as H

type FQN -- {{{1
  -- |A fully qualified name
  = String

data Variable -- {{{1
  -- |Local variables of functions, i.e. function parameters and automatic variables
  = Variable {
    var_decl  :: CDecl          -- ^the T-code declaration
  , var_fqn   :: FQN            -- ^the unique variable name
  , var_scope :: Maybe NodeInfo -- ^the node info of the surrounding T-code scope
  }

data Function -- {{{1
  -- |A critical function
  = Function {
    fun_vars  :: [Variable] -- ^the function's variables
  , fun_def   :: CFunDef    -- ^the original AST node
  , fun_body  :: Body       -- ^the function's body as graph of basic blocks
  }

fun_name :: Function -> Symbol -- {{{2
fun_name = symbol . fun_def

data Label -- {{{1
  -- |A label in the intermediate representation
  = TLabel Symbol H.Label -- ^A label originating from a T-code label
  | ILabel H.Label        -- ^A label required by the IR

instance Show Label where -- {{{2
  show (TLabel l _) = l
  show (ILabel l)   = show l

hLabel :: Label -> H.Label -- {{{2
hLabel (TLabel _ l) = l
hLabel (ILabel l)   = l

data Node e x where -- {{{1
  -- |Constitutes of basic blocks
  Label  :: Label                   -> Node C O  -- ^'lbl: ;'. Entry point to a basic block
  Stmt   :: CExpr                   -> Node O O  -- ^any expression. The only middle parts of basic blocks
  Goto   :: Label                   -> Node O C  -- ^'goto label;'
  If     :: CExpr -> Label -> Label -> Node O C  -- ^'if (cond) {goto label1;} else {goto label2;}'
  Call   :: CExpr -> Label          -> Node O C  -- ^a critical call in normal form. The label belongs to the subsequent basic block.
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
    . showString " then GOTO "
    . shows tl
    . showString " else GOTO "
    . shows el
    . showChar '\n'

  showsPrec _ (Call expr l) =
      showString "BLOCK #"
    . (shows . pretty) expr
    . showString "# GOTO "
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
