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
    tlocRow :: Int
  , tlocCol :: Int
  , tlocLen :: Int
  } deriving Show

data ELocation = ELocation { -- {{{2
    elocRow  :: Int
  , elocCol  :: Int
  , elocTid :: Maybe Int
  } deriving Show

type Location = (TLocation, ELocation) -- {{{2

type LocMap = [Location] -- {{{2

type Variable = String -- {{{2
type VarMap = [(Variable, Variable)] -- {{{2

data PreprocMap = PreprocMap { -- {{{2
    ppmMaxRow :: Int
  , ppmMapping :: [(Int, Int)]
  }

data File = File { -- {{{2
    fileName     :: FilePath
  , fileChecksum :: String
  }

data Thread = Thread { -- {{{2
    threadId        :: Int
  , threadStart    :: String
  , threadCritical :: [String]
  }
  
data DebugInfo = DebugInfo { -- {{{2
    diTcode   :: File
  , diPcode   :: BS.ByteString
  , diEcode   :: File
  , diPpm     :: PreprocMap
  , diLocMap  :: LocMap
  , diVarMap  :: VarMap
  , diThreads :: [Thread]
  , diOsApi   :: [String]
  }

map_preprocessed_row :: PreprocMap -> Int -> Maybe Int -- {{{1
map_preprocessed_row (PreprocMap rows locs) row
  | row <= 0 = Nothing
  | otherwise =
    let
      (src, dst) = (last . takeWhile ((<=row) . fst)) locs
      res = dst + (row - src) + 1
    in
      if res > rows
        then Nothing
        else Just res

-- instances {{{1
instance JSON TLocation where -- {{{2
  readJSON val = do
    [r,c,l] <- readJSON val
    return $ TLocation r c l

  showJSON (TLocation r c l) = showJSON [r, c, l]

instance JSON ELocation where -- {{{2
  readJSON val = do
    (r:c:t) <- readJSON val
    case t of
      [] -> return $ ELocation r c Nothing
      [t'] -> return $ ELocation r c (Just t')
      _ -> Error "wrong number of values on JSON array of ELocation type"

  showJSON (ELocation r c Nothing) = showJSON [r, c]
  showJSON (ELocation r c (Just t)) = showJSON [r, c, t]

instance JSON PreprocMap where  -- {{{2
  showJSON (PreprocMap mr ma) = (JSObject . toJSObject) [
        ("maxrow",  showJSON mr)
      , ("mapping", showJSON ma)
    ]

  readJSON (JSObject obj) = do
    let [mr, ma] = map snd $ fromJSObject obj
    mr' <- readJSON mr
    ma' <- readJSON ma
    return $ PreprocMap mr' ma' 

  readJSON x = readFail "PreprocMap" x

instance JSON File where -- {{{2
  readJSON val = readJSON val >>= \[n,c] -> return $ File n c
  showJSON (File n c) = showJSON [n, c]

instance JSON Thread where -- {{{2
  showJSON (Thread tid ts tc) = (JSObject . toJSObject) [
      ("id",       showJSON tid)
    , ("start",    showJSON ts)
    , ("critical", showJSON tc)
    ]

  readJSON (JSObject obj) = do
    let [tid, ts, tc] = map snd $ fromJSObject obj
    tid' <- readJSON tid
    ts'  <- readJSON ts
    tc'  <- readJSON tc
    return $ Thread tid' ts' tc'

  readJSON x = readFail "Thread" x

instance JSON DebugInfo where -- {{{2
  showJSON (DebugInfo tcode pcode ecode ppm lm vm ts oa) = (JSObject . toJSObject) [
      ("tcode",   showJSON tcode)
    , ("pcode",   showJSON pcode)
    , ("ecode",   showJSON ecode)
    , ("ppm",     showJSON ppm)
    , ("locmap",  showJSON lm)
    , ("varmap",  showJSON vm)
    , ("threads", showJSON ts)
    , ("osapi",   showJSON oa)
    ]

  readJSON (JSObject obj) = do
    let [tcode, pcode, ecode, ppm, lm, vm, ts, oa] = map snd $ fromJSObject obj
    [tcode', ecode'] <- mapM readJSON [tcode, ecode]
    pcode'           <- readJSON pcode
    ppm'             <- readJSON ppm
    lm'              <- readJSON lm
    vm'              <- readJSON vm
    ts'              <- readJSON ts
    oa'              <- readJSON oa
    return $ DebugInfo tcode' pcode' ecode' ppm' lm' vm' ts' oa'

  readJSON x = readFail "DebugInfo" x

-- utils {{{2
readFail :: String -> JSValue -> Result a
readFail type_ x = Error $ "unexpected JSON value for " ++ type_ ++ " type: '" ++ show x ++ "'"
