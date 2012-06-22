module Ocram.Ruab where

-- imports {{{1
import Text.JSON
import qualified Data.ByteString.Char8 as BS

encode_debug_info :: DebugInfo -> BS.ByteString -- {{{1
encode_debug_info = BS.pack . encodeStrict

decode_debug_info :: BS.ByteString -> Either String DebugInfo -- {{{1
decode_debug_info string = (resultToEither . decodeStrict . BS.unpack) string

-- types {{{1
data TLocation = TLocation {
    tlocRow :: Int
  , tlocCol :: Int
  , tlocLen :: Int
  } deriving (Show, Read)

data ELocation = ELocation {
    elocRow  :: Int
  , elocCol  :: Int
  , elocTidd :: Maybe Int
  }

data Location = Location TLocation ELocation

type LocMap = [Location]

type Variable = String
type VarMap = [(Variable, Variable)]

data File = File {
    fileName     :: FilePath
  , fileChecksum :: String
  }

data DebugInfo = DebugInfo {
    diTcode  :: File
  , diPcode  :: BS.ByteString
  , diEcode  :: File
  , diLocMap :: LocMap
  , diVarMap :: VarMap
  }

-- instances {{{1

instance JSON Location where
  readJSON val = do
    (tr:tc:tl:er:ec:tid) <- readJSON val
    case tid of
      [] -> return $ Location (TLocation tr tc tl) (ELocation er ec Nothing)
      [tid'] -> return $ Location (TLocation tr tc tl) (ELocation er ec (Just tid'))
      _ -> Error "wrong number of values in JSON array of Location type"

  showJSON (Location (TLocation tr tc tl) (ELocation er ec Nothing)) = showJSON [tr, tc, tl, er, ec]
  showJSON (Location (TLocation tr tc tl) (ELocation er ec (Just tid))) = showJSON [tr, tc, tl, er, ec, tid]


instance JSON File where
  readJSON val = readJSON val >>= \[n,c] -> return $ File n c
  showJSON (File n c) = showJSON [n, c]

instance JSON DebugInfo where
  showJSON (DebugInfo tcode pcode ecode lm vm) = (JSObject . toJSObject) [
      ("tcode", showJSON tcode)
    , ("pcode", showJSON pcode)
    , ("ecode", showJSON ecode)
    , ("locmap", showJSON lm)
    , ("varmap", showJSON vm)
    ]

  readJSON (JSObject obj) = do
    let [tcode, pcode, ecode, lm, vm] = map snd $ fromJSObject obj
    [tcode', ecode'] <- mapM readJSON [tcode, ecode]
    pcode' <- readJSON pcode
    lm' <- readJSON lm
    vm' <- readJSON vm
    return $ DebugInfo tcode' pcode' ecode' lm' vm'

  readJSON _ = Error "unexpected JSON value for DebugInfo type"

