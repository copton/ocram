module Ocram.Intermediate.Ast2Ir
-- exports {{{1
(
  ast2ir
) where

-- imports {{{1
import Compiler.Hoopl (C, O)
import Ocram.Symbols (Symbol)
import Language.C.Syntax.AST

import qualified Ocram.Intermediate.Representation as I
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Compiler.Hoopl as H

ast2ir = undefined

-- partition :: S.Set Symbol -> [CStat] -> M [Block]
-- partition cf items = 

-- isSplitPoint :: CStat -> Maybe SplitPoint
-- isSplitPoint (CIf _ _ _ _) = Just SplitAfter

data SplitPoint
  = SplitAt
  | SplitAfter

-- types {{{1
data ProtoBlock
  -- |Prototype of a basic block
  = Block {
      pbLabel :: I.Label -- ^The entry label of the block
    , pbItems :: [CStat] -- ^The body of the block
    }

data M a -- {{{1
  -- |The monad
  = M (LabelMap -> H.CheckingFuelMonad H.SimpleUniqueMonad (LabelMap, a))

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
