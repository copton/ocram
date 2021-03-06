{-# LANGUAGE TemplateHaskell, Rank2Types, KindSignatures #-}
module Ocram.Util
-- export {{{1
(
  (?:), (?++), tmap, trd, unexp, unexp', fromJust_s, head_s, tail_s, lookup_s, abort
) where

-- import {{{1
import Control.Arrow ((***), Arrow)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ()
import Language.C.Data.Node (undefNode, NodeInfo)
import Language.C.Pretty (Pretty, pretty)
import qualified Data.Map as Map

(?:) :: Maybe a -> [a] -> [a] -- {{{1
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:

(?++) :: Maybe [a] -> [a] -> [a]  -- {{{1
(Just xs) ?++ ys = xs ++ ys
Nothing   ?++ ys = ys
infixr 5 ?++

tmap :: Arrow a => a b c -> a (b, b) (c, c) -- {{{1
tmap f = f *** f

trd :: (a, b, c) -> c -- {{{1
trd (_, _, x) = x

unexp :: forall (f :: * -> *) a. -- {{{1
        (Pretty (f NodeInfo), Functor f) =>
        f a -> String
unexp x = "unexpected parameter: " ++ (show . pretty . fmap (const undefNode)) x

unexp' :: Pretty p => p -> String -- {{{1
unexp' x = "unexpected parameter: " ++ (show . pretty) x

fromJust_s :: Q Exp -- {{{1
fromJust_s = withLocatedError [| fromJust' |]

head_s :: Q Exp -- {{{1
head_s = withLocatedError [| head' |]

tail_s :: Q Exp -- {{{1
tail_s = withLocatedError [| tail' |]

lookup_s :: Q Exp -- {{{1
lookup_s = withLocatedError [| lookup' |]

abort :: Q Exp -- {{{1
abort = withLocatedError [| abort' |]

-- impl {{{1
fromJust' :: (String -> a) -> Maybe a -> a
fromJust' _ (Just x) = x
fromJust' err Nothing = err "fromJust failed"

head' :: (String -> a) -> [a] -> a
head' err xs
  | null xs = err "head failed"
  | otherwise = head xs

tail' :: (String -> [a]) -> [a] -> [a]
tail' err xs
  | null xs = err "tail failed"
  | otherwise = tail xs

lookup' :: (Show k, Ord k) => (String -> a) -> Map.Map k a -> k -> a
lookup' err map_ key =
  case Map.lookup key map_ of
    Nothing -> err $ "lookup failed: " ++ show key
    Just x -> x

abort' :: (String -> b) -> String -> b
abort' err what = err what

-- util {{{1
internalError :: String -> String -> a
internalError where_ what =
  let
    message = "internal error: " ++ where_ ++ ": " ++ what
    l = length message
    border = "+" ++ replicate (l+2) '-' ++ "+"
    text = "\n" ++ border ++ "\n| " ++ message ++ " |\n" ++ border ++ "\n"
  in
    error text

withLocatedError :: Q Exp -> Q Exp
withLocatedError f = do
    let err = locatedError =<< location
    appE f err

locatedError :: Loc -> Q Exp
locatedError loc = [| \what ->
  internalError $(litE $ stringL (formatLoc loc)) what |]

formatLoc :: Loc -> String
formatLoc loc =
  let
    file = loc_filename loc
    (line, col) = loc_start loc
  in
    concat [file, ":", show line, ":", show col]
