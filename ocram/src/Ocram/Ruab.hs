{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ocram.Ruab where

-- imports {{{1
import Text.JSON
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Language.C.Syntax.AST

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

encode_debug_info :: DebugInfo -> BS.ByteString -- {{{1
encode_debug_info = BS.pack . encodeStrict

decode_debug_info :: BS.ByteString -> Either String DebugInfo -- {{{1
decode_debug_info string = (resultToEither . decodeStrict . BS.unpack) string

-- types {{{1
newtype TRow -- {{{2
  = TRow {getTRow :: Int}
  deriving (Eq, Num, Ord)

newtype PRow -- {{{2
  = PRow {getPRow :: Int}
  deriving (Eq, Num, Ord)

newtype ERow -- {{{2
  = ERow {getERow :: Int}
  deriving (Eq, Num, Ord)

type ThreadId = Int

data TLocation = TLocation { -- {{{2
    tlocRow  :: TRow
  , tlocCol  :: Int
  , tlocLen  :: Int
  , tlocFile :: String
  } deriving (Show, Ord, Eq)

data ELocation = ELocation { -- {{{2
    elocRow  :: ERow
  , elocCol  :: Int
  } deriving Show

data LocKey = LocKey { -- {{{2
    locKeyThread :: Maybe ThreadId
  , locKeyTloc   :: TLocation
  } deriving (Eq, Ord)

newtype LocMap -- {{{2
  = LocMap { getLocMap :: M.Map LocKey [ELocation] }

data BlockingCall = BlockingCall { -- {{{2
    bcTloc     :: TLocation
  , bcEloc     :: ELocation
  , bcThreadId :: ThreadId
  } deriving Show

type BlockingCalls = [BlockingCall] -- {{{2

data Variable = Variable { -- {{{2
    varThread    :: ThreadId
  , varFunction  :: String
  , varSymbol    :: String
  } deriving (Ord, Eq)

newtype Substitution = Substitution { getSubstitution :: CExpr }

newtype VarMap
  = VarMap { getVarMap :: M.Map Variable Substitution }

data PreprocMap = PreprocMap { -- {{{2
    ppmMaxTRow :: TRow
  , ppmMaxPRow :: PRow
  , ppmMapping :: [(TRow, PRow)]
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
  , diLm        :: LocMap
  , diBcs       :: BlockingCalls
  , diVm        :: VarMap
  , diThreads   :: [Thread]
  , diOsApi     :: [String]
  , diCf        :: [String]
  }

-- instances {{{1
instance JSON TLocation where -- {{{2
  readJSON val = do
    ([r,c,l], f) <- readJSON val
    return $ TLocation (TRow r) c l f

  showJSON (TLocation (TRow r) c l f) = showJSON ([r, c, l], f)

instance JSON ELocation where -- {{{2
  readJSON val = do
    (r, c) <- readJSON val
    return $ ELocation (ERow r) c

  showJSON (ELocation (ERow r) c) = showJSON (r, c)

instance JSON LocKey where -- {{{2
  readJSON val = do
    (tid, tloc) <- readJSON val
    return $ LocKey (decodeTid tid) tloc

  showJSON (LocKey tid tloc) = showJSON (encodeTid tid, tloc)

instance JSON LocMap where -- {{{2
  readJSON val = (LocMap . M.fromList) <$> readJSON val

  showJSON = showJSON . M.toList . getLocMap

instance JSON Variable where -- {{{2
  readJSON val = do
    (tid, func, sym) <- readJSON val
    return $ Variable tid func sym

  showJSON (Variable tid func sym) = showJSON (tid, func, sym)

instance JSON Substitution where
  readJSON = undefined
  showJSON = undefined
    
instance JSON VarMap where
  readJSON val = VarMap . M.fromList <$> readJSON val
  
  showJSON = showJSON . M.toList . getVarMap

instance JSON BlockingCall where -- {{{2
  readJSON val = do
    (t, e, tid) <- readJSON val
    return $ BlockingCall t e tid

  showJSON (BlockingCall t e tid) = showJSON (t, e, tid)

instance JSON PreprocMap where  -- {{{2
  showJSON (PreprocMap (TRow mtr) (PRow mpr) ma) = (JSObject . toJSObject) [
        ("maxrow",  showJSON (mtr, mpr))
      , ("mapping", showJSON (map (getTRow *** getPRow) ma))
    ]

  readJSON (JSObject obj) = do
    let [mr, ma] = map snd $ fromJSObject obj
    (mtr, mpr) <- readJSON mr
    ma' <- readJSON ma
    return $ PreprocMap (TRow mtr) (PRow mpr) (map (TRow *** PRow) ma')

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
  showJSON (DebugInfo tcode pcode ecode ppm lm bcs vm ts oa cf) = (JSObject . toJSObject) [
      ("tcode",   showJSON tcode)
    , ("pcode",   showJSON pcode)
    , ("ecode",   showJSON ecode)
    , ("ppm",     showJSON ppm)
    , ("lm",      showJSON lm)
    , ("bcs",     showJSON bcs)
    , ("vs",      showJSON vm)
    , ("threads", showJSON ts)
    , ("osapi",   showJSON oa)
    , ("cf",      showJSON cf)
    ]

  readJSON (JSObject obj) = do
    let [tcode, pcode, ecode, ppm, lm, bcs, vm, ts, oa, cf] = map snd $ fromJSObject obj
    [tcode', ecode'] <- mapM readJSON [tcode, ecode]
    pcode'           <- readJSON pcode
    ppm'             <- readJSON ppm
    lm'              <- readJSON lm
    bcs'             <- readJSON bcs
    vm'              <- readJSON vm
    ts'              <- readJSON ts
    oa'              <- readJSON oa
    cf'              <- readJSON cf
    return $ DebugInfo tcode' pcode' ecode' ppm' lm' bcs' vm' ts' oa' cf'

  readJSON x = readFail "DebugInfo" x

instance Show TRow where -- {{{2
  show (TRow x) = show x

instance Show ERow where -- {{{2
  show (ERow x) = show x

instance Show PRow where -- {{{2
  show (PRow x) = show x

-- utils {{{2
readFail :: String -> JSValue -> Result a
readFail type_ x = Error $ "unexpected JSON value for " ++ type_ ++ " type: '" ++ show x ++ "'"

encodeTid :: Maybe ThreadId -> Int
encodeTid Nothing = -1
encodeTid (Just tid) = tid

decodeTid :: Int -> Maybe ThreadId
decodeTid (-1) = Nothing
decodeTid tid = Just tid
