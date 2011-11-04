import Data.Data
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Parser (parseC)
import Data.Generics
import Data.ByteString.Char8 (pack)
import Language.C.Data.Position (position)
import Control.Monad.State

count :: CStat -> Int
count (CIf _ _ _ _) = 1
count _ = 0

query :: CTranslUnit -> Int
query = everything (+) (mkQ 0 count)

rewriteReturn :: CStat -> State Int CStat
rewriteReturn (CReturn (Just (CConst _ )) _) = do
  idx <- get
  put (idx + 1)
  return (CReturn (Just (CConst (CIntConst (cInteger (fromIntegral idx)) undefNode))) undefNode)
rewriteReturn x = return x

rewrite :: CTranslUnit -> CTranslUnit
rewrite ast = fst $ runState (everywhereM (mkM rewriteReturn) ast) 0

remove x = everywhere (mkT removeDecl) x

removeDecl :: CStat -> CStat
removeDecl (CCompound x items y) = CCompound x (concatMap tr items) y
removeDecl x = x

tr :: CBlockItem -> [CBlockItem]
tr (CBlockDecl _) = []
tr x = [x]

parse :: String -> CTranslUnit
parse code = case parseC code' pos of 
  Left e -> error $ show e     
  Right ast -> ast             
  where 
    code' = pack code          
    pos = position 0 "<<test>>" 0 0 

code = parse "int x() {if (1) {int i; if(2) { foo: {int i2;} return 9;} return 23;}} int y() {if (1) {return 42;}}"

main = do
  print $ query code
  print "<<<<<<<<<<<<<<<<<<<<<<<<"
  print $ pretty $ rewrite code
  print "<<<<<<<<<<<<<<<<<<<<<<<<"
  print $ pretty $ remove code
