import CreateContext
import Ana2Types
import Context

main = putStrLn $ show $ get $ ctxResult2 ctx
    where
    ctx = createContext 23
    get (Result2 i) = i
