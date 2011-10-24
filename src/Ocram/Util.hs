module Ocram.Util (
	(?:), tmap, trd
) where

import Control.Arrow ((***))

(?:) :: Maybe a -> [a] -> [a]
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:


tmap f = f *** f


trd :: (a, b, c) -> c
trd (_, _, x) = x
