import Control.Monad.State

data Stmt = Stmt | NestedStmt [Stmt] | Call Int
data FunDef = FunDef Int Stmt
data TranslUnit = TranslUnit [FunDef]

example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

class Visitor a where
    handleFunctionDefinition :: FunDef -> a -> a

data UserState = UserState {
    fbCount :: Int,
    stmtCount :: Int
} deriving Show

instance Visitor UserState where 
    handleFunctionDefinition fd (UserState c c') = UserState (c+1) c'

travTranslUnit :: Visitor v => TranslUnit -> State v ()
travTranslUnit (TranslUnit fds) = mapM_ travFunDef fds

travFunDef :: Visitor v => FunDef -> State v ()
travFunDef fd@(FunDef id stmt) = do
    modify $ handleFunctionDefinition fd
--    travStmt stmt
    return ()

main :: IO ()
main = putStrLn $ show $ snd $ runState (travTranslUnit example) (UserState 0 0)
