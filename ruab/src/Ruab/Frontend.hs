{-# LANGUAGE TemplateHaskell, ViewPatterns, ScopedTypeVariables #-}
module Ruab.Frontend
-- exports {{{1
(
  run
) where

-- imports {{{1
import Control.Applicative ((<$>))
import Data.List (intercalate, find)
import Data.Maybe (fromJust, listToMaybe)
import Graphics.UI.Gtk hiding (response)
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Reactive.Banana (actuate, compile, reactimate, NetworkDescription, newEvent, accumE, union, liftIO, filterJust)
import Ruab.Frontend.Infos (setHighlight, InfoInstance, render_info, setBreakpoint, infoIsHighlight, setThread)
import Ruab.Frontend.Reactive (event_input)
import Ruab.Options (Options)
import Ruab.Util (fromJust_s, abort)

import qualified Ruab.Core as C
import qualified Data.ByteString.Char8 as BS

run :: Options -> IO () -- {{{1
run opt = do
  gui <- loadGui
  core <- C.setup opt
  let ctx = Context gui core
  setupGui ctx
  network <- compile $ createNetwork ctx opt
  actuate network
  mainGUI

-- types {{{1
data GUI = GUI { -- {{{2
    guiWin     :: Window
  , guiTcomp   :: Component
  , guiPcomp   :: Component
  , guiEcomp   :: Component
  , guiLog     :: TextView
  , guiView    :: TextView
  , guiInput   :: Entry
  , guiStatus  :: Statusbar
  }

data Component = Component { -- {{{2
    compCode  :: TextView
  , compInfo  :: TextView
  , compLines :: TextView
  , compLabel :: Label
  , compView  :: Viewport
  }

data Context = Context { -- {{{2
    ctxGUI  :: GUI
  , ctxCore :: C.Context
  }

type InfoEvent = [InfoInstance] -> [InfoInstance] -- {{{2

data CommandPrefix -- {{{2
  = CmdBreakAdd
  | CmdBreakList
  | CmdContinue
  | CmdHelp
  | CmdInterrupt
  | CmdQuit
  | CmdScroll
  | CmdStart

data Command -- {{{2
  = CmdEvUnknown String
  | CmdEvBreakAdd C.PRow
  | CmdEvBreakList
  | CmdEvContinue
  | CmdEvHelp (Maybe CommandPrefix)
  | CmdEvInterrupt
  | CmdEvQuit
  | CmdEvScroll RowType Int
  | CmdEvStart

data RowType -- {{{2
  = Tcode | Pcode | Ecode

data Log = Log LogType [String] -- {{{2

data LogType -- {{{2
  = LogOutput
  | LogError
  | LogStatus
  | LogPrompt

-- semantics {{{1
instance Read CommandPrefix where -- {{{2
  readsPrec _ command = case lookup command commands of
    Nothing -> []
    Just cmd -> [(cmd, "")]

commands :: [(String, CommandPrefix)] -- {{{2
commands = [
    ("badd", CmdBreakAdd)
  , ("blist", CmdBreakList)
  , ("continue", CmdContinue)
  , ("help", CmdHelp)
  , ("interrupt", CmdInterrupt)
  , ("quit", CmdQuit)
  , ("scroll", CmdScroll)
  , ("start", CmdStart)
  ]

instance Read Command where -- {{{2
  readsPrec _ command = [(parseCommand command, "")]

parseCommand :: String -> Command -- {{{2
parseCommand text =
  let (command:options) = words text in
  case mread command of
    Nothing -> CmdEvUnknown command
    Just cmd -> case cmd of
      CmdBreakAdd -> case options of
        [row@(mread -> Just (_ :: Int))] -> CmdEvBreakAdd $ (fromJust . mread) row
        _ -> CmdEvHelp (Just CmdBreakAdd)

      CmdBreakList -> noopt CmdBreakList CmdEvBreakList options
      
      CmdContinue -> noopt CmdContinue CmdEvContinue options

      CmdHelp -> case options of
        [cmd'] -> case mread cmd' of
          Nothing -> CmdEvUnknown cmd'
          Just cmd'' -> CmdEvHelp (Just cmd'')
        _ -> CmdEvHelp Nothing

      CmdInterrupt -> noopt CmdInterrupt CmdEvInterrupt options

      CmdQuit -> noopt CmdQuit CmdEvQuit options

      CmdScroll -> case options of
        [rtype@((`elem`["t","p","e"]) -> True), row@(mread -> Just (_ :: Int))] ->
          CmdEvScroll (read rtype)  ((fromJust . mread) row)
        _ -> CmdEvHelp (Just CmdScroll)

      CmdStart -> noopt CmdStart CmdEvStart options

  where
    noopt _   ev [] = ev
    noopt cmd _  _  = CmdEvHelp (Just cmd)

    mread x = listToMaybe [y | (y,"") <- reads x] 

help :: CommandPrefix -> [String] -- {{{2
help CmdInterrupt = ["interrupt: interrupt execution"]
help CmdContinue  = ["continue: continue execution"]
help CmdBreakAdd  = ["badd prow: add a breakpoint"]
help CmdBreakList = ["blist: list all breakpoints"]
help CmdQuit = ["quit: quit ruab"]
help CmdScroll = ["scroll t|p|e row: scroll views to the given row"]
help CmdHelp = ["help: show the list of available commands"]
help CmdStart = ["start: start execution"]

instance Read RowType where -- {{{2
  readsPrec _ rtype = case lookup rtype rowtypes of
    Nothing -> []
    Just rt -> [(rt, "")]
    where
      rowtypes = [
          ("t", Tcode)
        , ("p", Pcode)
        , ("e", Ecode)
        ]

instance Show LogType where -- {{{2
  show LogOutput = ">"
  show LogError = "!"
  show LogStatus = "@"
  show LogPrompt = "$"

-- event network {{{1
type Fire e = e -> IO () -- {{{2

data InternalEvent = InternalEvent { -- {{{2
      evLog     :: Maybe Log
    , evUpdate  :: Maybe Update
    , evCommand :: Maybe C.Command
    , evAction  :: Maybe (IO ())
  }

type State = [InfoInstance] -- {{{2

type Update = State -> State -- {{{2

createNetwork :: Context -> Options -> NetworkDescription t () -- {{{2
createNetwork (Context gui core) opt = do
  eInput <- event_input (guiInput gui)
  (eQuit, fQuit) <- newEvent
  (eResponse, fResponse) <- newEvent
  (eStatus, fStatus) <- newEvent
  fCommand <- C.create_network core opt fResponse fStatus

  _ <- liftIO $ onDestroy (guiWin gui) (fQuit CmdEvQuit)

  let
    eInternal = 
              (handleCommand core <$> ((read <$> eInput) `union` eQuit))
      `union` (handleResponse <$> eResponse)
      `union` (handleStatus <$> eStatus)
    (eLog, eUpdate, eCommand, eAction) = split eInternal

    infos = foldr (flip setBreakpoint 0) [] $ C.possible_breakpoints core
    ePinfo = accumE infos eUpdate
    eTinfo = sync (C.p2t_row core) <$> ePinfo
    eEinfo = sync (C.p2e_row core) <$> ePinfo

  -- send commands
  reactimate $ fCommand <$> eCommand
  -- show log output
  reactimate $ log (guiLog gui) <$> eLog `union` (Log LogPrompt . (:[]) <$> eInput)
  -- update infos
  reactimate $ render (guiTcomp gui) <$> eTinfo
  reactimate $ render (guiPcomp gui) <$> ePinfo
  reactimate $ render (guiEcomp gui) <$> eEinfo
  -- internal event actions
  reactimate $ eAction
    where
      split e = (filterJust $ evLog <$> e, filterJust $ evUpdate <$> e, filterJust $ evCommand <$> e, filterJust $ evAction <$> e)

log :: TextView -> Log -> IO () -- {{{2
log tv (Log lt lines) = postGUIAsync $ do
  let
    prefix = show lt ++ " "
    text = (intercalate "\n" $ map (prefix++) lines) ++ "\n"
  buffer <- textViewGetBuffer tv
  end <- textBufferGetEndIter buffer
  textBufferInsert buffer end text
  mark <- textBufferGetMark buffer "append"
  textViewScrollToMark tv ($fromJust_s mark) 0 Nothing 
  return ()

render :: Component -> State -> IO () -- {{{2
render comp infos = postGUIAsync $ do
  setText (compInfo comp) (render_info infos)
  maybe (return ()) (scrollToRow comp . fst) $ find (infoIsHighlight . snd) infos

sync :: (C.PRow -> Maybe Int) -> State -> State -- {{{2
sync f infos = flip map infos $ \(prow, x) ->
  case f prow of
    Nothing -> $abort $ "failed to map row: " ++ show prow
    Just row' -> (row', x)

handleStatus :: C.Status -> InternalEvent
handleStatus status =
  let
    threads = C.statusThreads status
    updateInfo = foldr updateThread id threads
    updateThread thread f = case C.thProw thread of
      Nothing -> f
      Just prow -> f . setThread prow (C.thId thread)
  in InternalEvent
      (Just $ Log LogStatus $ map show threads)
      (Just updateInfo)
      Nothing
      Nothing

handleResponse :: C.Response -> InternalEvent -- {{{2
handleResponse = either (lerror . (:[])) handle . snd
  where
    handle (C.ResAddBreakpoint bp) = InternalEvent
        (Just $ Log LogOutput ["breakpoint added", show bp])
        (Just $ setBreakpoint (C.breakpointRow bp) (C.breakpointNumber bp))
        Nothing
        Nothing

    handle C.ResContinue = loutput ["continued"]

    handle C.ResInterrupt = loutput ["interrupted"]

    handle (C.ResListBreakpoints bps) = loutput $ "breakpoints:" : map show bps

    handle C.ResShutdown = InternalEvent Nothing Nothing Nothing (Just mainQuit)

    handle C.ResStart = loutput ["started"]

handleCommand :: C.Context -> Command -> InternalEvent -- {{{2
handleCommand core = handle
  where
    handle (CmdEvUnknown cmd) = lerror ["unknown command '" ++ cmd ++ "'", "type 'help' for assistance"]

    handle (CmdEvBreakAdd prow) = command $ C.CmdAddBreakpoint prow

    handle CmdEvBreakList = command C.CmdListBreakpoints

    handle CmdEvContinue = command C.CmdContinue

    handle CmdEvInterrupt = command C.CmdInterrupt

    handle CmdEvQuit = command C.CmdShutdown

    handle (CmdEvHelp Nothing) = loutput $ -- {{{3
         "available commands:"
        : intercalate ", " (map fst commands)
        : "type 'help <command>' for more information about <command>"
        : []

    handle (CmdEvHelp (Just cmd)) = loutput (help cmd)

    handle (CmdEvScroll rt srow) = -- {{{3
      let
        f = case rt of     
          Tcode -> C.t2p_row core
          Pcode -> Just
          Ecode -> C.e2p_row core
      in
        case f srow of
          Nothing -> lerror ["invalid row number"]
          Just prow -> InternalEvent Nothing (Just $ setHighlight prow) Nothing Nothing

    handle CmdEvStart = command C.CmdStart

    -- utils {{{3
    command cmd = InternalEvent Nothing Nothing (Just cmd) Nothing
    
-- utils {{{2
logInternal :: LogType -> [String] -> InternalEvent
logInternal t ss = InternalEvent (Just $ Log t ss) Nothing Nothing Nothing

loutput, lerror :: [String] -> InternalEvent
loutput = logInternal LogOutput 
lerror = logInternal LogError

-- setup and shutdown {{{1
loadGui :: IO GUI -- {{{2
loadGui = do
  gladefn <- getDataFileName "ruab.glade"
  _ <- initGUI
  Just xml <- xmlNew gladefn
  window <- xmlGetWidget xml castToWindow "window"
  [ct, cp, ce] <- mapM (loadComponent xml) ["t", "p", "e"]
  [log', view] <- mapM (xmlGetWidget xml castToTextView) ["log", "view"]
  input <- xmlGetWidget xml castToEntry "input"
  status <- xmlGetWidget xml castToStatusbar "status"
  return $ GUI window ct cp ce log' view input status
  where
    loadComponent xml comp = do
      [code, info, lines] <- mapM (xmlGetWidget xml castToTextView) $ map (comp++) ["code", "info", "lines"]
      label <- xmlGetWidget xml castToLabel (comp++"label")
      view <- xmlGetWidget xml castToViewport (comp++"view")
      return $ Component code info lines label view

setupGui :: Context -> IO () -- {{{2
setupGui (Context gui core) = do
  setupComponent (guiTcomp gui)
    (C.t_code core)
    ("(T-code) " ++ C.t_file core)

  setupComponent (guiPcomp gui)
    (C.p_code core)
    "(pre-processed)"

  setupComponent (guiEcomp gui)
    (C.e_code core)
    ("(E-code) " ++ C.e_file core)

  setupText (guiView gui) (BS.pack "")
  setupText (guiLog gui) (BS.pack "")
  addAppendMarker (guiLog gui)
  widgetGrabFocus (guiInput gui)
  widgetShowAll (guiWin gui)

  where
    addAppendMarker tv = do -- {{{3
      buffer <- textViewGetBuffer tv
      end <- textBufferGetEndIter buffer
      _ <- textBufferCreateMark buffer (Just "append") end False 
      return ()

    setupComponent comp code caption = do -- {{{3
      setupText (compCode comp) code
      setupText (compInfo comp) (BS.pack "")
      let rows = BS.lines code
      let range = [1..(length rows)]
      let text = BS.intercalate (BS.singleton '\n') $ map (BS.pack . show) range
      setupText (compLines comp) text
      labelSetText (compLabel comp) caption

    setupText tv txt = do -- {{{3
      buffer <- textBufferNew Nothing
      textBufferInsertByteStringAtCursor buffer txt
      textViewSetBuffer tv buffer

-- utils {{{1
scrollToRow :: Component -> Int -> IO () -- {{{2
scrollToRow comp row = do
  buffer <- textViewGetBuffer (compCode comp)
  maxRow <- textBufferGetLineCount buffer
  adj <- viewportGetVAdjustment (compView comp)
  upper <- adjustmentGetUpper adj
  lower <- adjustmentGetLower adj
  pageSize <- adjustmentGetPageSize adj
  let ratio = fromIntegral (row - 1) / fromIntegral (maxRow - 1)
  let range = (upper - pageSize) - lower
  adjustmentSetValue adj $ ratio * range + lower
  viewportSetVAdjustment (compView comp) adj

setText :: TextView -> String -> IO () -- {{{2
setText tv txt = do
  buffer <- textViewGetBuffer tv
  textBufferSetText buffer txt


