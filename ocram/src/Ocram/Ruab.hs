module Ocram.Ruab where

-- imports {{{1
import Text.JSON

import qualified Data.ByteString.Char8 as BS

encode_debug_info :: DebugInfo -> BS.ByteString -- {{{1
encode_debug_info = BS.pack . encodeStrict

decode_debug_info :: BS.ByteString -> Either String DebugInfo -- {{{1
decode_debug_info string = (resultToEither . decodeStrict . BS.unpack) string

-- types {{{1
data TLocation = TLocation { -- {{{2
    tlocRow  :: Int
  , tlocCol  :: Int
  , tlocLen  :: Int
  , tlocFile :: String
  } deriving Show

data ELocation = ELocation { -- {{{2
    elocRow  :: Int
  , elocCol  :: Int
  } deriving Show

data Breakpoint = Breakpoint { -- {{{2
    bpTloc           :: TLocation
  , bpEloc           :: ELocation
  , bpThreadId       :: Maybe Int
  } deriving (Show)

type Breakpoints = [Breakpoint] -- {{{2

data BlockingCall = BlockingCall { -- {{{2
    bcEloc     :: ELocation
  , bcThreadId :: Int
  }

type BlockingCalls = [BlockingCall] -- {{{2

type Variable = String -- {{{2

type VarMap = [(Variable, Variable)] -- {{{2

data PreprocMap = PreprocMap { -- {{{2
    ppmMaxTRow :: Int
  , ppmMaxPRow :: Int
  , ppmMapping :: [(Int, Int)]
  }

data File = File { -- {{{2
    fileName     :: FilePath
  , fileChecksum :: String
  }

data Thread = Thread { -- {{{2
    threadId        :: Int
  , threadStart     :: String
  , threadExecution :: String
  , threadCritical  :: [String]
  } deriving Show
  
data DebugInfo = DebugInfo { -- {{{2
    diTcode     :: File
  , diPcode     :: BS.ByteString
  , diEcode     :: File
  , diPpm       :: PreprocMap
  , diBps       :: Breakpoints
  , diBcs       :: BlockingCalls
  , diVarMap    :: VarMap
  , diThreads   :: [Thread]
  , diOsApi     :: [String]
  }

-- instances {{{1
instance JSON TLocation where -- {{{2
  readJSON val = do
    ([r,c,l], f) <- readJSON val
    return $ TLocation r c l f

  showJSON (TLocation r c l f) = showJSON ([r, c, l], f)

instance JSON ELocation where -- {{{2
  readJSON val = readJSON val >>= return . uncurry ELocation

  showJSON (ELocation r c) = showJSON (r, c)

instance JSON Breakpoint where -- {{{2
  readJSON val = do
    (t, e, tid) <- readJSON val
    return $ Breakpoint t e (if tid == -1 then Nothing else Just tid)

  showJSON (Breakpoint t e tid) = showJSON (t, e, maybe (-1) id tid)

instance JSON BlockingCall where -- {{{2
  readJSON val = readJSON val >>= return . uncurry BlockingCall

  showJSON (BlockingCall e t) = showJSON (e, t)

instance JSON PreprocMap where  -- {{{2
  showJSON (PreprocMap mtr mpr ma) = (JSObject . toJSObject) [
        ("maxrow",  showJSON (mtr, mpr))
      , ("mapping", showJSON ma)
    ]

  readJSON (JSObject obj) = do
    let [mr, ma] = map snd $ fromJSObject obj
    (mtr, mpr) <- readJSON mr
    ma' <- readJSON ma
    return $ PreprocMap mtr mpr ma' 

  readJSON x = readFail "PreprocMap" x

instance JSON File where -- {{{2
  readJSON val = readJSON val >>= \[n,c] -> return $ File n c
  showJSON (File n c) = showJSON [n, c]

instance JSON Thread where -- {{{2
  showJSON (Thread tid ts te tc) = (JSObject . toJSObject) [
      ("id",       showJSON tid)
    , ("start",    showJSON ts)
    , ("execute" , showJSON te)
    , ("critical", showJSON tc)
    ]

  readJSON (JSObject obj) = do
    let [tid, ts, te, tc] = map snd $ fromJSObject obj
    tid' <- readJSON tid
    ts'  <- readJSON ts
    te'  <- readJSON te
    tc'  <- readJSON tc
    return $ Thread tid' ts' te' tc'

  readJSON x = readFail "Thread" x

instance JSON DebugInfo where -- {{{2
  showJSON (DebugInfo tcode pcode ecode ppm bps bcs vm ts oa) = (JSObject . toJSObject) [
      ("tcode",   showJSON tcode)
    , ("pcode",   showJSON pcode)
    , ("ecode",   showJSON ecode)
    , ("ppm",     showJSON ppm)
    , ("bps",     showJSON bps)
    , ("bcs",     showJSON bcs)
    , ("vs",      showJSON vm)
    , ("threads", showJSON ts)
    , ("osapi",   showJSON oa)
    ]

  readJSON (JSObject obj) = do
    let [tcode, pcode, ecode, ppm, bps, bcs, vm, ts, oa] = map snd $ fromJSObject obj
    [tcode', ecode'] <- mapM readJSON [tcode, ecode]
    pcode'           <- readJSON pcode
    ppm'             <- readJSON ppm
    bps'              <- readJSON bps
    bcs'              <- readJSON bcs
    vm'              <- readJSON vm
    ts'              <- readJSON ts
    oa'              <- readJSON oa
    return $ DebugInfo tcode' pcode' ecode' ppm' bps' bcs' vm' ts' oa'

  readJSON x = readFail "DebugInfo" x

-- utils {{{2
readFail :: String -> JSValue -> Result a
readFail type_ x = Error $ "unexpected JSON value for " ++ type_ ++ " type: '" ++ show x ++ "'"
