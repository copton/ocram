module Ocram.Ruab where

data TLocation = TLocation {
    tlocRow :: Int
  , tlocCol :: Int
  , tlocLen :: Int
  } deriving (Show, Read)

data ELocation = ELocation {
    elocRow  :: Int
  , elocCol  :: Int
  , elocTidd :: Maybe Int
  } deriving (Show, Read)

data Location = Location TLocation ELocation deriving (Show, Read)

type LocMap = [Location]

type Variable = String
type VarMap = [(Variable, Variable)]

data File = File {
    fileName     :: FilePath
  , fileChecksum :: String
  } deriving (Show, Read)

data DebugInfo = DebugInfo {
    diTcode  :: File
  , diEcode  :: File
  , diLocMap :: LocMap
  , diVarMap :: VarMap
  } deriving (Show, Read)

