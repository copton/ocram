module Ocram.Util (
	(?:), tmap, trd, fromJust_s, head_s, abort
) where

import Control.Arrow ((***))

(?:) :: Maybe a -> [a] -> [a]
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:


tmap f = f *** f


trd :: (a, b, c) -> c
trd (_, _, x) = x


fromJust_s :: String -> Maybe a -> a
fromJust_s _ (Just x) = x
fromJust_s location Nothing = abort $ "fromJust failed: " ++ location


head_s :: String -> [a] -> a
head_s _ (x:_) = x
head_s location _ = abort $ "head failed: " ++ location


abort :: String -> a
abort string =
  let
    header = "internal error: "
    message = header ++ string
    l = length message
    border = "+" ++ (replicate (l+2) '-') ++ "+"
    text = "\n" ++ border ++ "\n| " ++ message ++ " |\n" ++ border ++ "\n"
  in
    error text
  
