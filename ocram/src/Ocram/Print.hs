-- {{{1
-- The Pretty instances of this module are taken from Language.C
-- http://hackage.haskell.org/package/language-c
-- Here is their copyright notice:
--
-- Copyright (c) 1999-2008 Manuel M T Chakravarty
--                                                 Duncan Coutts
--                                   Benedikt Huber
-- Portions Copyright (c)  1989,  1990  James  A.  Roskind
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-- }}}1
{-# LANGUAGE FlexibleInstances, TemplateHaskell, MultiParamTypeClasses #-}
module Ocram.Print
-- export {{{1
(
  print_with_log, pretty
) where

-- import {{{1
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid)
import Language.C.Data.Position (posRow)
import Language.C.Syntax
import Language.C.Data.Ident (Ident, identToString)
import Language.C.Data.Node (posOfNode, isUndefNode, NodeInfo)
import Text.PrettyPrint
import Ocram.Debug (ENodeInfo(..), Breakpoint(..), Breakpoints)
import Ocram.Ruab (ERow(..), TRow(..))
import Ocram.Util (abort)
import Prelude hiding (log)

import qualified Data.ByteString.Char8 as BS

print_with_log :: (Monoid m, PrettyLog a m) => a -> (BS.ByteString, m) -- {{{1
print_with_log o =
  let
    (code, log) = renderWithLog (pretty o)
  in
    (BS.pack code, log)

marker :: (Monoid m, MarkerInfo i m) => i -> DocL m -> DocL m -- {{{2
marker mi doc
  | addMarker mi = here (getLogger mi) doc
  | otherwise    = doc
    
class Monoid m => PrettyLog a m where -- {{{2
  pretty :: a -> DocL m
  prettyPrec :: Int -> a -> DocL m

  pretty = prettyPrec 0
  prettyPrec _ = pretty

class Monoid m => MarkerInfo a m where -- {{{2
  addMarker :: a -> Bool
  getLogger :: a -> Logger m

instance MarkerInfo NodeInfo () where
  addMarker = const False
  getLogger = undefined

instance MarkerInfo ENodeInfo [Breakpoint] where
  addMarker = (&&) <$> not . isUndefNode . enTnodeInfo <*> enBreakpoint
  getLogger eni (Position r _) = [Breakpoint trow (ERow r) (enThreadId eni) (enBlockingCall eni)]
    where
      trow = TRow . posRow . posOfNode . enTnodeInfo $ eni

-- utils {{{2
maybeP :: (p -> DocL m) -> Maybe p -> DocL m -- {{{3
-- pretty print optional chunk
maybeP = maybe empty

ifP :: Bool -> DocL m -> DocL m -- {{{3
-- pretty print when flag is true
ifP flag doc = if flag then doc else empty

mlistP :: ([p] -> DocL m) -> [p] -> DocL m -- {{{3
-- pretty print _optional_ list, i.e. [] ~ Nothing and (x:xs) ~ Just (x:xs)
mlistP pp xs = maybeP pp (if null xs then Nothing else Just xs)

identP :: Ident -> DocL m -- {{{3
-- pretty print identifier
identP = text . identToString

attrlistP :: (Monoid m, MarkerInfo i m) => [CAttribute i] -> DocL m -- {{{3
-- pretty print attribute annotations
attrlistP [] = empty
attrlistP attrs = text "__attribute__" <> parens (parens (hcat . punctuate comma . map pretty $ attrs))

parenPrec :: Monoid m => Int -> Int -> DocL m -> DocL m -- {{{3
-- analogous to showParen
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

ii :: DocL m -> DocL m -- {{{3
-- indent a chunk of code
ii = nest 4

binPrec :: CBinaryOp -> Int -- {{{3
-- precedence of C operators
binPrec CMulOp = 20
binPrec CDivOp = 20
binPrec CRmdOp = 20
binPrec CAddOp = 19
binPrec CSubOp = 19
binPrec CShlOp = 18
binPrec CShrOp = 18
binPrec CLeOp  = 17
binPrec CGrOp  = 17
binPrec CLeqOp = 17
binPrec CGeqOp = 17
binPrec CEqOp  = 16
binPrec CNeqOp = 16
binPrec CAndOp = 15
binPrec CXorOp = 14
binPrec COrOp  = 13
binPrec CLndOp = 12
binPrec CLorOp = 11

-- PrettyLog instances {{{2
instance (Monoid m, MarkerInfo i m) => PrettyLog (CTranslationUnit i) m where -- {{{3
  pretty (CTranslUnit edecls ni) = marker ni $ vcat (map pretty edecls)

-- TODO: Check need of __extension__
instance (Monoid m, MarkerInfo i m) => PrettyLog (CExternalDeclaration i) m where -- {{{3
  pretty (CDeclExt decl) = pretty decl <> semi
  pretty (CFDefExt fund) = pretty fund
  pretty (CAsmExt  asmStmt ni) = marker ni $ text "asm" <> parens (pretty asmStmt) <> semi

instance (Monoid m, MarkerInfo i m) => PrettyLog (CFunctionDef i) m where -- {{{3
-- TODO: Check that old-style and new-style aren't mixed
  pretty (CFunDef declspecs declr decls stat ni) = marker ni $ 
          hsep (map pretty declspecs)                         
      <+> pretty declr                                       
      $+$ (ii . vcat . map (<> semi) . map pretty) decls    
      $$ prettyPrec (-1) stat                              
                                                          

instance (Monoid m, MarkerInfo i m) => PrettyLog (CStatement i) m where -- {{{3
  pretty (CLabel ident stat cattrs ni)        = marker ni $ identP ident <> text ":" <+> attrlistP cattrs $$ pretty stat

  pretty (CCase expr stat ni)                 = marker ni $ text "case" <+> pretty expr <> text ":" $$ pretty stat

  pretty (CCases expr1 expr2 stat ni)         = marker ni $ text "case" <+> pretty expr1 <+> text "..." <+> pretty expr2 <> text ":" $$ pretty stat

  pretty (CDefault stat ni)                   = marker ni $ text "default:" $$ pretty stat

  pretty (CExpr expr ni)                      = ii $ marker ni $ maybeP pretty expr <> semi

  pretty c@(CCompound _ _ ni)                 = marker ni $ prettyPrec 0 c

  pretty (CIf expr stat estat ni)             = ii $ marker ni $ text "if" <+> parens (pretty expr) $+$ prettyBody stat $$  maybeP prettyElse estat
    where
      prettyBody c@(CCompound _ _ ni') = marker ni' $ prettyPrec (-1) c
      prettyBody nonCompound         = prettyPrec (-1) (CCompound [] [CBlockStmt nonCompound] ni)
      prettyElse (CIf else_if_expr else_if_stat else_stat ni') = marker ni' $
        text "else if" <+> parens (pretty else_if_expr)
          $+$ prettyBody else_if_stat
          $$  maybeP prettyElse else_stat
      prettyElse else_stmt =
        text "else"
          $+$ prettyBody else_stmt

  pretty (CSwitch expr stat ni)               = ii $ marker ni $ text "switch" <+> text "(" <> pretty expr <> text ")" $+$ prettyPrec (-1) stat

  pretty (CWhile expr stat False ni)          = ii $ marker ni $ text "while" <+> text "(" <> pretty expr <> text ")" $+$ prettyPrec (-1) stat

  pretty (CWhile expr stat True ni)           = ii $ marker ni $ text "do" $+$ prettyPrec (-1) stat $$ text "while" <+> text "(" <> pretty expr <> text ");"

  pretty (CFor for_init cond step stat ni)    = ii $ marker ni $ text "for" <+> text "(" <> either (maybeP pretty) pretty for_init <> semi <+> maybeP pretty cond <> semi <+> maybeP pretty step <> text ")" $+$ prettyPrec (-1) stat 

  pretty (CGoto ident ni)                     = ii $ marker ni $ text "goto" <+> identP ident <> semi

  pretty (CGotoPtr expr ni)                   = ii $ marker ni $ text "goto" <+> text "*" <+> prettyPrec 30 expr <> semi

  pretty (CCont ni)                           = ii $ marker ni $ text "continue" <> semi

  pretty (CBreak ni)                          = ii $ marker ni $ text "break" <> semi

  pretty (CReturn Nothing ni)                 = ii $ marker ni $ text "return" <> semi

  pretty (CReturn (Just e) ni)                = ii $ marker ni $ text "return" <+> pretty e <> semi

  pretty (CAsm asmStmt ni)                    = marker ni $ pretty asmStmt

  prettyPrec p (CCompound localLabels bis ni) = marker ni $
      let inner = text "{" $+$ mlistP ppLblDecls localLabels $+$ vcat (map pretty bis) $$ text "}" in
      if p == -1 then inner else ii inner
      where ppLblDecls =  vcat . map (\l -> text "__label__" <+> identP l <+> semi)

  prettyPrec _ p                              = pretty p

instance (Monoid m, MarkerInfo i m) => PrettyLog (CAssemblyStatement i) m where -- {{{3
    pretty (CAsmStmt tyQual expr outOps inOps clobbers ni) = marker ni $
        ii $ text "__asm__" <+>
             maybeP pretty tyQual <>
             parens asmStmt <> semi
      where
        asmStmt = pretty expr <+>
                  (if all null [inOps,outOps] && null clobbers then empty else ops)
        ops     =  text ":" <+> hcat (punctuate comma (map pretty outOps)) <+>
                   text ":" <+> hcat (punctuate comma (map pretty inOps)) <+>
                   (if null clobbers then empty else clobs)
        clobs   =  text ":" <+> hcat (punctuate comma (map pretty clobbers))

instance (Monoid m, MarkerInfo i m) => PrettyLog (CAssemblyOperand i) m where -- {{{3
    -- asm_operand :~ [operand-name] "constraint" ( expr )
    pretty (CAsmOperand mArgName cnstr expr ni) = marker ni $
        maybeP (\argName -> text "[" <> identP argName <> text "]") mArgName <+>
        pretty cnstr <+>
        parens (pretty expr)

-- TODO: Check need of __extension__
instance (Monoid m, MarkerInfo i m) => PrettyLog (CCompoundBlockItem i) m where -- {{{3
    pretty (CBlockStmt stat)      = pretty stat
    pretty (CBlockDecl decl)      = ii $ pretty decl <> semi
    pretty (CNestedFunDef fundef) = ii $ pretty fundef

instance (Monoid m, MarkerInfo i m) => PrettyLog (CDeclaration i) m where -- {{{3
    -- CAVEAT:
    -- we may not print __attribute__s directly after typespecs,
    -- as this may change the semantics of the declaration.
    -- The parser fixes this, but to avoid hard-to-track code generator
    -- errors, we enforce this invariant on the AST level.
    pretty (CDecl specs divs ni) = marker ni $
        hsep (map pretty checked_specs) <+> hsep (punctuate comma (map p divs))
            where
            -- possible hint for AST improvement - (declr, initializer, expr, attrs)
            -- currently there are no sensible attributes for unnamed bitfields though
            p (declr, initializer, expr) =
                maybeP (prettyDeclr False 0) declr <+>
                maybeP ((text ":" <+>) . pretty) expr <+>
                attrlistP (getAttrs declr) <+>
                maybeP ((text "=" <+>) . pretty) initializer
            checked_specs =
                case any isAttrAfterSUE  (zip specs (tail specs)) of
                    True -> $abort $
                              ("Warning: AST Invariant violated: __attribute__ specifier following struct/union/enum:"++
                               (show $ map pretty specs))
                    False -> specs
            isAttrAfterSUE (CTypeSpec ty,CTypeQual (CAttrQual _)) = isSUEDef ty
            isAttrAfterSUE _ = False
            getAttrs Nothing = []
            getAttrs (Just (CDeclr _ _ _ cattrs _)) = cattrs

instance (Monoid m, MarkerInfo i m) => PrettyLog (CDeclarationSpecifier i) m where -- {{{3
    pretty (CStorageSpec sp)  = pretty sp
    pretty (CTypeSpec sp)     = pretty sp
    pretty (CTypeQual qu)     = pretty qu

instance (Monoid m, MarkerInfo i m) => PrettyLog (CStorageSpecifier i) m where -- {{{3
    pretty (CAuto ni)     = marker ni $ text "auto"
    pretty (CRegister ni) = marker ni $ text "register"
    pretty (CStatic ni)   = marker ni $ text "static"
    pretty (CExtern ni)   = marker ni $ text "extern"
    pretty (CTypedef ni)  = marker ni $ text "typedef"
    pretty (CThread ni)   = marker ni $ text "__thread"

instance (Monoid m, MarkerInfo i m) => PrettyLog (CTypeSpecifier i) m where -- {{{3
    pretty (CVoidType ni)        = marker ni $ text "void"
    pretty (CCharType ni)        = marker ni $ text "char"
    pretty (CShortType ni)       = marker ni $ text "short"
    pretty (CIntType ni)         = marker ni $ text "int"
    pretty (CLongType ni)        = marker ni $ text "long"
    pretty (CFloatType ni)       = marker ni $ text "float"
    pretty (CDoubleType ni)      = marker ni $ text "double"
    pretty (CSignedType ni)      = marker ni $ text "signed"
    pretty (CUnsigType ni)       = marker ni $ text "unsigned"
    pretty (CBoolType ni)        = marker ni $ text "_Bool"
    pretty (CComplexType ni)     = marker ni $ text "_Complex"
    pretty (CSUType union ni)    = marker ni $ pretty union
    pretty (CEnumType enum ni)   = marker ni $ pretty enum
    pretty (CTypeDef ident ni)   = marker ni $ identP ident
    pretty (CTypeOfExpr expr ni) = marker ni $ text "typeof" <> text "(" <> pretty expr <> text ")"
    pretty (CTypeOfType decl ni) = marker ni $ text "typeof" <> text "(" <> pretty decl <> text ")"

instance (Monoid m, MarkerInfo i m) => PrettyLog (CTypeQualifier i) m where -- {{{3
    pretty (CConstQual ni)  = marker ni $ text "const"
    pretty (CVolatQual ni)  = marker ni $ text "volatile"
    pretty (CRestrQual ni)  = marker ni $ text "__restrict"
    pretty (CInlineQual ni) = marker ni $ text "inline"
    pretty (CAttrQual a)    = attrlistP [a]

instance (Monoid m, MarkerInfo i m) => PrettyLog (CStructureUnion i) m where -- {{{3
    pretty (CStruct tag ident Nothing cattrs ni)      = marker ni $ pretty tag <+> attrlistP cattrs <+> maybeP identP ident
    pretty (CStruct tag ident (Just []) cattrs ni)    = marker ni $ pretty tag <+> attrlistP cattrs <+> maybeP identP ident <+> text "{ }"
    pretty (CStruct tag ident (Just decls) cattrs ni) = marker ni $ vcat [pretty tag <+> attrlistP cattrs <+> maybeP identP ident <+> text "{", ii $ sep (map (<> semi) (map pretty decls)), text "}"]

instance Monoid m => PrettyLog CStructTag m where -- {{{3
    pretty CStructTag = text "struct"
    pretty CUnionTag  = text "union"

instance (Monoid m, MarkerInfo i m) => PrettyLog (CEnumeration i) m where -- {{{3
  pretty (CEnum enum_ident Nothing cattrs ni)     = marker ni $ text "enum" <+> attrlistP cattrs <+> maybeP identP enum_ident
  pretty (CEnum enum_ident (Just vals) cattrs ni) = marker ni $ vcat [text "enum" <+> attrlistP cattrs <+> maybeP identP enum_ident <+> text "{", ii $ sep (punctuate comma (map p vals)), text "}"]
    where
      p (ident, expr) = identP ident <+> maybeP ((text "=" <+>) . pretty) expr

--  {{{3
--  Analyze a declarator and return a human-readable description 
--   See C99 Spec p 115ff.
-- describeDeclr :: CDeclr -> Doc
-- describeDeclr declr =
--     let declrs = reverse (declrChain declr) in
--     endDescr (foldl descrDeclr undefined declrs)
--
--   where
--   declrChain declr@(CVarDeclr _ _ _ _) = [declr]
--   declrChain declr@(CPtrDeclr _ ideclr _)   = declr : declrChain ideclr
--   declrChain declr@(CArrDeclr ideclr _ _ _) = declr : declrChain ideclr
--   declrChain declr@(CFunDeclr ideclr _ _ _)   = declr : declrChain ideclr
--
--   descrDeclr _ (CVarDeclr ident asm cattrs _) = single False $ \_ ->
--       maybe (text "<anonymous>") identP ident <+>
--       maybeP (\asmname -> parens (text "asm:" <+> pretty asmname)) asm <+>
--       text "is" <+> (if null cattrs then empty else prettyList (map CAttrQual cattrs) <> comma)
--   descrDeclr (pre,isPlural) (CPtrDeclr quals declr _) = single isPlural $ \pluralize ->
--       pre <+> indefArticle isPlural <> prettyList quals <+> pluralize "pointer to" "pointers to"
--   descrDeclr (pre,isPlural) (CArrDeclr declr quals expr _) = plural isPlural $ \pluralize ->
--       pre <+> indefArticle' isPlural <> prettyList quals <+> pluralize "array of" "arrays of"
--   descrDeclr (pre,isPlural) (CFunDeclr declr params cattrs _) = single isPlural $ \pluralize ->
--       pre <+> indefArticle isPlural <> prettyList (map CAttrQual cattrs) <+> pluralize "function returning" "functions returning"
--   endDescr (pre, isPlural) =  pre <+> text (if isPlural then "<typed objects>" else "a <typed object>")
--   single :: Bool -> ( (String -> String -> Doc) -> a ) -> (a, Bool)
--   single isPlural mkDescr = (mkDescr (pluralize isPlural), isPlural)
--   plural :: Bool -> ( (String -> String -> Doc) -> a ) -> (a, Bool)
--   plural isPlural mkDescr = (mkDescr (pluralize isPlural), True)
--   indefArticle isPlural  = text$ if isPlural then "" else "a "
--   indefArticle' isPlural = text$ if isPlural then "" else "an "
--   pluralize isPlural s p = text (if isPlural then p else s)
--   prettyList :: (Pretty a) => [a] -> Doc
--   prettyList = hsep . punctuate comma . map pretty

instance (Monoid m, MarkerInfo i m) => PrettyLog (CDeclarator i) m where -- {{{3
    prettyPrec prec declr = marker (annotation declr) $ prettyDeclr True prec declr

prettyDeclr :: (Monoid m, MarkerInfo i m) => Bool -> Int -> CDeclarator i -> DocL m -- {{{4
prettyDeclr show_attrs prec (CDeclr name derived_declrs asmname cattrs _) =
    ppDeclr prec (reverse derived_declrs) <+> prettyAsmName asmname <+> ifP show_attrs (attrlistP cattrs)
    where
    ppDeclr _ [] = maybeP identP name
    --'*' __attribute__? qualifiers declarator
    ppDeclr p (CPtrDeclr quals _ : declrs) =
        parenPrec p 5 $ text "*" <+> hsep (map pretty quals) <+> ppDeclr 5 declrs

    -- declarator[ __attribute__? qualifiers expr ]
    ppDeclr p (CArrDeclr quals size _ : declrs) =
        parenPrec p 6 $ ppDeclr 6 declrs <> brackets (hsep (map pretty quals) <+> pretty size)
    -- declarator ( arguments )
    -- or (__attribute__ declarator) (arguments)
    ppDeclr _ (CFunDeclr params fun_attrs _ : declrs) =
        (if not (null fun_attrs) then parens (attrlistP fun_attrs <+> ppDeclr 5 declrs) else ppDeclr 6 declrs)
        <> parens (prettyParams params)
    prettyParams (Right (decls, isVariadic)) =
     sep (punctuate comma (map pretty decls))
     <> (if isVariadic then text "," <+> text "..." else empty)
    prettyParams (Left oldStyleIds) =
     hsep (punctuate comma (map identP oldStyleIds))
    prettyAsmName asm_name_opt
        = maybe empty (\asm_name -> text "__asm__" <> parens (pretty asm_name)) asm_name_opt

instance (Monoid m, MarkerInfo i m) => PrettyLog (CArraySize i) m where -- {{{3
  pretty (CNoArrSize completeType) = ifP completeType (text "*")
  pretty (CArrSize staticMod expr) = ifP staticMod (text "static") <+> pretty expr
-- initializer :: { CInit }
-- initializer :- assignment_expression
--              | '{' (designation? initializer)_cs_list '}'

instance (Monoid m, MarkerInfo i m) => PrettyLog (CInitializer i) m where -- {{{3
  pretty (CInitExpr expr ni)  = marker ni $ pretty expr
  pretty (CInitList initl ni) = marker ni $ text "{" <+> hsep (punctuate comma (map p initl)) <+> text "}"
    where
    p ([], initializer)     = pretty initializer
    p (desigs, initializer) = hsep (map pretty desigs) <+> text "=" <+> pretty initializer

-- designation :- designator_list '='
--             | array_range_designator
-- arr_designator :- '[' constant_expression ']'
-- member_designator :-  '.' identifier
-- arr_range _designator :- '[' constant_expression "..." constant_expression ']'

instance (Monoid m, MarkerInfo i m) => PrettyLog (CPartDesignator i) m where -- {{{3
    pretty (CArrDesig expr ni)          = marker ni $ text "[" <> pretty expr <> text "]"
    pretty (CMemberDesig ident ni)      = marker ni $ text "." <> identP ident
    pretty (CRangeDesig expr1 expr2 ni) = marker ni $ text "[" <> pretty expr1 <+> text "..." <+> pretty expr2 <> text "]"

instance (Monoid m, MarkerInfo i m) => PrettyLog (CAttribute i) m where -- {{{3
    pretty (CAttr attrName [] ni)         = marker ni $ identP attrName
    pretty (CAttr attrName attrParams ni) = marker ni $ identP attrName <> parens (hsep . punctuate comma . map pretty $ attrParams)

instance (Monoid m, MarkerInfo i m) => PrettyLog (CExpression i) m where -- {{{3
    prettyPrec p (CComma exprs ni)                  = marker ni $ parenPrec p (-1) $ hsep (punctuate comma (map (prettyPrec 2) exprs))

    prettyPrec p (CAssign op expr1 expr2 ni)        = marker ni $ parenPrec p 2 $ prettyPrec 3 expr1 <+> pretty op <+> prettyPrec 2 expr2

    -- NB: assignment only has a higher precedence if cond is on the rhs
    prettyPrec p (CCond expr1 expr2 expr3 ni)       = marker ni $ parenPrec p 2 $ prettyPrec 4 expr1 <+> text "?" <+> maybeP pretty expr2 <+> text ":" <+> prettyPrec 4 expr3

    prettyPrec p (CBinary op expr1 expr2 ni)        = marker ni $ parenPrec p prec $ prettyPrec prec expr1 <+> pretty op <+> prettyPrec (prec + 1) expr2
      where prec = binPrec op

    prettyPrec p (CCast decl expr ni)               = marker ni $ parenPrec p 25 $ text "(" <> pretty decl <> text ")" <+> prettyPrec 25 expr

    prettyPrec p (CUnary CPostIncOp expr ni)        = marker ni $ parenPrec p 26 $ prettyPrec 26 expr <> text "++"

    prettyPrec p (CUnary CPostDecOp expr ni)        = marker ni $ parenPrec p 26 $ prettyPrec 26 expr <> text "--"

    prettyPrec p (CUnary op expr@(CUnary _ _ _) ni) = marker ni $ parenPrec p 25 $ pretty op <+> parens (prettyPrec 25 expr) -- parens aren't necessary, but look nicer imho

    prettyPrec p (CUnary op expr ni)                = marker ni $ parenPrec p 25 $ pretty op <> prettyPrec 25 expr

    prettyPrec p (CSizeofExpr expr ni)              = marker ni $ parenPrec p 25 $ text "sizeof" <> parens (pretty expr)

    prettyPrec p (CSizeofType decl ni)              = marker ni $ parenPrec p 25 $ text "sizeof" <> parens (pretty decl)

    prettyPrec p (CAlignofExpr expr ni)             = marker ni $ parenPrec p 25 $ text "__alignof" <> parens (pretty expr)

    prettyPrec p (CAlignofType decl ni)             = marker ni $ parenPrec p 25 $ text "__alignof" <> parens (pretty decl)

    prettyPrec p (CComplexReal expr ni)             = marker ni $ parenPrec p 25 $ text "__real" <+> prettyPrec 25 expr

    prettyPrec p (CComplexImag expr ni)             = marker ni $ parenPrec p 25 $ text "__imag" <+> prettyPrec 25 expr
    
    prettyPrec p (CIndex expr1 expr2 ni)            = marker ni $ parenPrec p 26 $ prettyPrec 26 expr1 <> text "[" <> pretty expr2 <> text "]"

    prettyPrec p (CCall expr args ni)               = marker ni $ parenPrec p 30 $ prettyPrec 30 expr <> text "(" <> (sep . punctuate comma . map pretty) args <> text ")"

    prettyPrec p (CMember expr ident deref ni)      = marker ni $ parenPrec p 26 $ prettyPrec 26 expr <> text (if deref then "->" else ".") <> identP ident

    prettyPrec _p (CVar ident ni)                   = marker ni $ identP ident

    prettyPrec _p (CConst constant)                 = pretty constant

    prettyPrec _p (CCompoundLit decl initl ni)      = marker ni $ parens (pretty decl) <+> (braces . hsep . punctuate comma) (map p initl)
      where
        p ([], initializer)           = pretty initializer
        p (mems, initializer) = hcat (punctuate (text ".") (map pretty mems)) <+> text "=" <+> pretty initializer

    prettyPrec _p (CStatExpr stat ni)               = marker ni $ text "(" <> pretty stat <> text ")"

    -- unary_expr :- && ident  {- address of label -}
    prettyPrec _p (CLabAddrExpr ident _) = text "&&" <> identP ident

    prettyPrec _p (CBuiltinExpr builtin) = pretty builtin

instance (Monoid m, MarkerInfo i m) => PrettyLog (CBuiltinThing i) m where -- {{{3
    pretty (CBuiltinVaArg expr ty_name ni)                                = marker ni $ text "__builtin_va_arg" <+> (parens $ pretty expr <> comma <+> pretty ty_name)

    -- The first desig has to be a member field.
    pretty (CBuiltinOffsetOf ty_name (CMemberDesig field1 _ : desigs) ni) = marker ni $ text "__builtin_offsetof" <+> (parens $ pretty ty_name <> comma <+> identP field1 <> hcat (map pretty desigs) )

    pretty (CBuiltinOffsetOf _ty_name otherDesigs _)                      = error $ "Inconsistent AST: Cannot interpret designators in offsetOf: "++ show (hcat$ map pretty otherDesigs)

    pretty (CBuiltinTypesCompatible ty1 ty2 ni)                           = marker ni $ text "__builtin_types_compatible_p" <+> (parens $ pretty ty1 <> comma <+> pretty ty2)

instance Monoid m => PrettyLog CAssignOp m where -- {{{3
  pretty op = text $ case op of
    CAssignOp -> "="
    CMulAssOp -> "*="
    CDivAssOp -> "/="
    CRmdAssOp -> "%="
    CAddAssOp -> "+="
    CSubAssOp -> "-="
    CShlAssOp -> "<<="
    CShrAssOp -> ">>="
    CAndAssOp -> "&="
    CXorAssOp -> "^="
    COrAssOp  -> "|="

instance Monoid m => PrettyLog CBinaryOp m where -- {{{3
  pretty op = text $ case op of
    CMulOp -> "*"
    CDivOp -> "/"
    CRmdOp -> "%"
    CAddOp -> "+"
    CSubOp -> "-"
    CShlOp -> "<<"
    CShrOp -> ">>"
    CLeOp  -> "<"
    CGrOp  -> ">"
    CLeqOp -> "<="
    CGeqOp -> ">="
    CEqOp  -> "=="
    CNeqOp -> "!="
    CAndOp -> "&"
    CXorOp -> "^"
    COrOp  -> "|"
    CLndOp -> "&&"
    CLorOp -> "||"

instance Monoid m => PrettyLog CUnaryOp m where -- {{{3
  pretty op = text $ case op of
    CPreIncOp  -> "++"
    CPreDecOp  -> "--"
    CPostIncOp -> "++"
    CPostDecOp -> "--"
    CAdrOp     -> "&"
    CIndOp     -> "*"
    CPlusOp    -> "+"
    CMinOp     -> "-"
    CCompOp    -> "~"
    CNegOp     -> "!"

instance (Monoid m, MarkerInfo i m) => PrettyLog (CConstant i) m where -- {{{3
    pretty (CIntConst   int_const ni) = marker ni $ text (show int_const)
    pretty (CCharConst  chr ni)       = marker ni $ text (show chr)
    pretty (CFloatConst flt ni)       = marker ni $ text (show flt)
    pretty (CStrConst   str ni)       = marker ni $ text (show str)

instance (Monoid m, MarkerInfo i m) => PrettyLog (CStringLiteral i) m where -- {{{3
    pretty (CStrLit   str _) = text (show str)

