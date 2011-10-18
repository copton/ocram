module Ocram.Util (
	(?:)
) where


(?:) :: Maybe a -> [a] -> [a]
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:
