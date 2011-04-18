module Ocram.Util (
	(?:), mapt2
) where


(?:) :: Maybe a -> [a] -> [a]
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:

mapt2 :: (a,b) -> ((a->c),(b->d)) -> (c,d)
mapt2 (x, y) (f, g) = (f x, g y)
