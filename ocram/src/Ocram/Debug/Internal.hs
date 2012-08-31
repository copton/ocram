{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Ocram.Debug.Internal where

import Data.Maybe (mapMaybe)
import Language.C.Syntax.AST (CTranslUnit, CTranslationUnit(CTranslUnit), CExternalDeclaration(CFDefExt), annotation)
import Ocram.Ruab (PreprocMap(..), TRow(..), FunMap(..))
import Ocram.Symbols (symbol)
import Language.C.Data.Position (posRow)
import Language.C.Data.Node (posOfNode, getLastTokenPos)
import Ocram.Util (abort)
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

preproc_map :: BS.ByteString -> BS.ByteString -> PreprocMap -- {{{1
preproc_map tcode pcode
  | BS.null pcode = PreprocMap 0 0 []
  | BS.last pcode /= '\n' = $abort "pre-processed file should be terminated by a newline"
  | otherwise = PreprocMap (trows) (prows - 1) (reverse ppm)
  where
    trows = TRow $ length $ BS.split '\n' tcode
    (first:rest) = init $ BS.split '\n' pcode
    (ppm, prows) = foldl go ([], 2) rest
    mainFile = case match first of
      (_, (BS.null -> True), _, _) -> $abort $ "unexpected first row in pre-processed file"
      (_, _, _, (_:file:_)) -> file
      x -> $abort $ "unexpected parameter: " ++ show x

    match :: BS.ByteString -> (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
    match txt = txt =~ "^# ([0-9]*) \"([^\"]+)\".*$" -- prefix, match, postfix, groups

    go (ppm', row) line = case match line of
      (_, (BS.null -> True), _, _ ) -> (ppm', row + 1)
      (_, _, _, (row':file:_)) -> if file == mainFile
        then (((TRow . read . BS.unpack) row', row) : ppm', row + 1)
        else (ppm', row + 1)
      x -> $abort $ "unexpected parameter:" ++ show x

fun_map :: CTranslUnit -> FunMap -- {{{1
fun_map (CTranslUnit ds _) = (FunMap . M.fromList . mapMaybe extend) ds
  where
    extend (CFDefExt fd) = 
      let
        ni = annotation fd
        start = (TRow . posRow . posOfNode) ni
        end = (TRow . posRow . fst . getLastTokenPos) ni
      in
        Just (symbol fd, (start, end))
     
    extend _ = Nothing
