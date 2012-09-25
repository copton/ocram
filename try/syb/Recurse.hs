{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}

import Data.Data (Data, gmapM)
import Data.Typeable (Typeable)
import Data.Generics (mkQ, mkM, GenericQ, GenericM)

data D 
  = A Int
  | B D D
  deriving (Show, Data, Typeable)

trav :: D -> IO D
trav o = print o >> return o

exclude :: D -> Bool
exclude (B _ _) = True
exclude _       = False

topDownButM :: (Monad m, Data a) => GenericQ Bool -> GenericM m -> a -> m a
topDownButM q f x
  | q x == False = return x
  | otherwise = do
    x' <- f x
    gmapM (topDownButM q f) x'

main :: IO ()
main = topDownButM (mkQ False exclude) (mkM trav) (B (A 23) (A 42)) >> return ()
