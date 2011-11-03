import Language.C.Syntax.AST
import Language.C.Parser (parseC)
import Data.Generics
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (position)

count :: CStat -> Int
count (CIf _ _ _ _) = 1
count _ = 0

query :: CTranslUnit -> Int
query = everything (+) (mkQ 0 count)

parse :: String -> CTranslUnit
parse code = case parseC code' pos of 
  Left e -> error $ show e     
  Right ast -> ast             
  where 
    code' = pack code          
    pos = position 0 "<<test>>" 0 0 

main = putStrLn $ show $ query $ parse "int x() {if (1) {return 23;}} int y() {if (1) {return 23;}} "
