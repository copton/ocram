{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ocram.Ruab where

-- imports {{{1
import Text.JSON
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (guard)

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


type MapPE -- {{{2
  -- |Map between P-rows and E-rows
  = [(PLocation, [ERow])]

data PLocation = PLocation { -- {{{3
  -- |A location in the P-code
    plThread       :: Maybe ThreadId
  , plRow          :: PRow
  , plBlockingCall :: Bool
  } deriving (Eq, Ord)

data MapTP = -- {{{2
  -- |Map between T-rows and P-rows
  MapTP {
    mtpMaxTRow :: TRow
  , mtpMaxPRow :: PRow
  , mtpMapping :: [(TRow, PRow)]
  }

type VarMap -- {{{2
  -- |Mapping of Variable names
  = [(Scope, RenameMap)]

data Scope =  -- {{{3
  Scope {
    scStart :: PRow
  , scEnd   :: PRow
  } deriving (Eq, Ord)

type RenameMap = [(Variable, FQN)] -- {{{3

data Variable -- {{{3
  = AutomaticVariable {
    varThread :: ThreadId
  , varTName  :: String
  }
  | StaticVariable {
    varTName  :: String
  }  

type FQN -- {{{3
  -- |A fully qualified name of a variable
  = String

type StepMap = M.Map -- {{{2
    PRow             -- current row
    [(  
        PRow         -- next row
      , Bool         -- next row is in different function?
    )]

data File = File { -- {{{2
  -- |Information about a source file
    fileName     :: FilePath
  , fileChecksum :: String
  }

data Thread = Thread { -- {{{2
  -- |Information about a T-thread
    threadId        :: Int
  , threadStart     :: String
  , threadExecution :: String
  , threadCritical  :: [String]
  } deriving Show
  
data DebugInfo = DebugInfo { -- {{{2
  -- |The debugging information that is exchanged between Ocram and Ruab
    diTcode     :: File
  , diPcode     :: BS.ByteString
  , diEcode     :: File
  , diMtp       :: MapTP
  , diMpe       :: MapPE
  , diVm        :: VarMap
  , diSm        :: StepMap
  , diThreads   :: [Thread]
  , diOsApi     :: [String]
  }

-- instances {{{1
instance JSON TRow where -- {{{2
  readJSON val = TRow <$> readJSON val
  showJSON = showJSON . getTRow

instance JSON PRow where -- {{{2
  readJSON val = PRow <$> readJSON val
  showJSON = showJSON . getPRow

instance JSON ERow where -- {{{2
  readJSON val = ERow <$> readJSON val
  showJSON = showJSON . getERow

instance JSON PLocation where -- {{{2
  readJSON val = do
    (t, r, b) <- readJSON val
    return $ PLocation (decodeTid t) r b

  showJSON (PLocation t r b) = showJSON (encodeTid t, r, b)


instance JSON MapTP where  -- {{{2
  readJSON (JSObject obj) = do
    let [mr, ma] = map snd $ fromJSObject obj
    (mtr, mpr) <- readJSON mr
    ma' <- readJSON ma
    return $ MapTP (TRow mtr) (PRow mpr) (map (TRow *** PRow) ma')

  readJSON x = readFail "MapTP" x

  showJSON (MapTP (TRow mtr) (PRow mpr) ma) = (JSObject . toJSObject) [
        ("max",  showJSON (mtr, mpr))
      , ("map", showJSON (map (getTRow *** getPRow) ma))
    ]

instance JSON Scope where  -- {{{2
  readJSON val = do
    (s, e) <- readJSON val
    return $ Scope s e

  showJSON (Scope s e) = showJSON (s, e)

instance JSON Variable where -- {{{2
  readJSON val = do
    (c, o) <- readJSON val
    case c :: Int of
      0 -> do
        (t, n) <- readJSON o
        return $ AutomaticVariable t n
      1 -> do
        n <- readJSON o
        return $ StaticVariable n
      _ -> readFail "Variable" val

  showJSON (AutomaticVariable t n) = showJSON (0 :: Int, showJSON (t, n))
  showJSON (StaticVariable n)      = showJSON (1 :: Int, showJSON n)


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
  showJSON (DebugInfo tcode pcode ecode mtp mpe vm sm ts api) = (JSObject . toJSObject) [
      ("tcode",   showJSON tcode)
    , ("pcode",   showJSON pcode)
    , ("ecode",   showJSON ecode)
    , ("mtp",     showJSON mtp)
    , ("mpe",     showJSON mpe)
    , ("vm",      showJSON vm)
    , ("sm",      showJSON sm)
    , ("threads", showJSON ts)
    , ("api",     showJSON api)
    ]

  readJSON (JSObject obj) = do
    let [tcode, pcode, ecode, mtp, mpe, vm, sm, ts, api] = map snd $ fromJSObject obj
    [tcode', ecode'] <- mapM readJSON [tcode, ecode]
    pcode'           <- readJSON pcode
    mtp'             <- readJSON mtp
    mpe'             <- readJSON mpe
    vm'              <- readJSON vm
    sm'              <- readJSON sm
    ts'              <- readJSON ts
    api'             <- readJSON api
    return $ DebugInfo tcode' pcode' ecode' mtp' mpe' vm' sm' ts' api'

  readJSON x = readFail "DebugInfo" x

instance Show TRow where -- {{{2
  show (TRow x) = show x

instance Show ERow where -- {{{2
  show (ERow x) = show x

instance Show PRow where -- {{{2
  show (PRow x) = show x

-- utils {{{1
readFail :: String -> JSValue -> Result a
readFail type_ x = Error $ "unexpected JSON value for " ++ type_ ++ " type: '" ++ show x ++ "'"

encodeTid :: Maybe ThreadId -> Int
encodeTid Nothing = -1
encodeTid (Just tid) = tid

decodeTid :: Int -> Maybe ThreadId
decodeTid (-1) = Nothing
decodeTid tid = Just tid

-- mapper {{{1
t2p_row :: MapTP -> TRow -> Maybe PRow -- {{{2
t2p_row (MapTP _ prows locs) trow = do
  guard (trow > 0)
  let (src, dst) = (last . takeWhile ((<=trow) . fst)) locs
  let prow = dst + (PRow . getTRow) (trow - src + 1)
  guard (prow <= prows)
  return prow

p2t_row :: MapTP -> PRow -> Maybe TRow -- {{{2
p2t_row mtp@(MapTP trows _ locs) prow = do
  guard (prow > 0)
  let (src, dst) = (last . takeWhile ((<=prow) . snd)) locs
  let trow = src + (TRow . getPRow) (prow - dst - 1)
  guard (trow <= trows)
  prow' <- t2p_row mtp trow
  guard (prow' == prow)
  return trow

