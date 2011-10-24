-- rewrite declarations of blocking functions
-- remove critical functions
module Ocram.Transformation.Inline.Step1
-- exports {{{1
(
  step1
) where

-- imports {{{1
import Control.Monad.Reader (ask)
import Data.Monoid
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Ocram.Analysis (CallGraph, is_blocking, is_critical)
import Ocram.Symbols (symbol)
import Ocram.Transformation.Inline.Names
import Ocram.Transformation.Inline.Types
import Ocram.Transformation.Util (un, ident)
import Ocram.Types (Ast)
import Ocram.Visitor
import qualified Data.Map as Map
import qualified Data.Set as Set

-- step1 :: Ast -> WR (Ast) {{{1
step1 :: Ast -> WR (Ast)
step1 ast = do
  cg <- ask
  return $ fst $ (traverseCTranslUnit ast cg :: (Ast, ()))

-- Visitor {{{2
instance DownVisitor CallGraph

instance UpVisitor CallGraph () where
  -- rewrite function declarations of blocking functions
  upCExtDecl o@(CDeclExt cd) cg _
    | is_blocking cg (symbol cd) = (cd', ())
    | otherwise = (o, mempty)
    where
      cd' = CDeclExt $ createBlockingFunctionDeclr cd

  upCExtDecl o _ u = (o, u)

instance ListVisitor CallGraph () where
  -- remove critical functions
  nextCExtDecl o@(CFDefExt fd) cg u
    | is_critical cg (symbol fd) = ([], cg, ())
    | otherwise = ([o], cg, u)
  
  nextCExtDecl o d u = ([o], d, u)

-- createBlockingFunctionDeclr :: CDecl -> CDecl {{{2
createBlockingFunctionDeclr :: CDecl -> CDecl
createBlockingFunctionDeclr cd = let fName = symbol cd in
   CDecl [CTypeSpec (CVoidType un)] [(Just (CDeclr (Just (ident fName)) [CFunDeclr (Right ([CDecl [CTypeSpec (CTypeDef (ident (frameType fName)) un)] [(Just (CDeclr (Just (ident frameParam)) [CPtrDeclr [] un] Nothing [] un), Nothing, Nothing)] un], False)) [] un] Nothing [] un), Nothing, Nothing)] un


