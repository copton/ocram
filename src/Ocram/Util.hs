module Ocram.Util (
	(?:), mapt2
) where


(?:) :: Maybe a -> [a] -> [a]
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:

mapt2 :: ((a->c),(b->d)) -> (a,b) -> (c,d)
mapt2 (f, g) (x, y) = (f x, g y)
