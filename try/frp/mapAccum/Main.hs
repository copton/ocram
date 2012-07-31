module Main where

import Reactive.Banana

foo :: Event t (Int -> (Int, Int)) -> Event t Int
foo = fst . mapAccum 0

main = do
  bar <- interpretModel foo $ map (:[]) [(\x -> (undefined,x+1))]
  putStrLn . show $ bar
