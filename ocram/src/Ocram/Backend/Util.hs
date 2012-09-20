{-# LANGUAGE TemplateHaskell #-}
module Ocram.Backend.Util where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Language.C.Data.Ident (internalIdent)
import Language.C.Syntax.AST
import Ocram.Intermediate
import Ocram.Util (abort, unexp)

makeVarDecl :: Variable -> CDecl -- {{{2
makeVarDecl = renameDecl <$> var_unique <*> var_decl
  where
    renameDecl newName (CDecl x1 [(Just declr, x2, x3)] x4) =
      CDecl x1 [(Just (renameDeclr newName declr), x2, x3)] x4
    renameDecl _ x = $abort $ unexp x

    renameDeclr newName (CDeclr (Just _) x1 x2 x3 x4) =
      CDeclr (Just (internalIdent newName)) x1 x2 x3 x4
    renameDeclr _ x = $abort $ unexp x
