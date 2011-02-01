import Control.Monad.State

data Stmt = Stmt | NestedStmt [Stmt] | Call Int
data FunDef = FunDef Int Stmt
data TranslUnit = TranslUnit [FunDef]

example = TranslUnit([
   FunDef 1 Stmt
 , FunDef 2 (NestedStmt [Call 1])
    ])

data UserState = UserState {
    fbCount :: Int,
    stmtCount :: Int
} deriving Show

funCb :: FunDef -> UserState -> UserState
funCb fd (UserState c c') = UserState (c+1) c'

travTranslUnit :: TranslUnit -> State UserState ()
travTranslUnit (TranslUnit fds) = mapM_ travFunDef fds

travFunDef :: FunDef -> State UserState ()
travFunDef fd@(FunDef id stmt) = do
    modify $ funCb fd
--    travStmt stmt
    return ()

main :: IO ()
main = putStrLn $ show $ snd $ runState (travTranslUnit example) (UserState 0 0)
