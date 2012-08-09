{-# LANGUAGE TemplateHaskell, ViewPatterns, ScopedTypeVariables #-}
module Ruab.Frontend
-- exports {{{1
(
  run
) where

-- imports {{{1
import Control.Arrow (first)
import Data.List (intercalate, find, isPrefixOf)
import Data.Maybe (fromJust, listToMaybe)
import Graphics.UI.Gtk hiding (response)
import Graphics.UI.Gtk.Glade (xmlNew, xmlGetWidget)
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Paths_Ruab (getDataFileName)
import Prelude hiding (log, lines)
import Ruab.Actor (new_actor, update)
import Ruab.Frontend.Infos (setHighlight, InfoInstance, render_info, setBreakpoint, infoIsHighlight, setThread)
import Ruab.Options (Options)
import Ruab.Util (fromJust_s)

import qualified Ruab.Core as C
import qualified Data.ByteString.Char8 as BS

run :: Options -> IO () -- {{{1
run opt = do
  gui <- loadGui
  core <- C.setup opt
  let ctx = Context gui core
  setupGui ctx
  _ <- createNetwork ctx opt
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

data InputState = InputState { -- {{{2
    inputHistory :: [String]
  , inputFuture :: [String]
  }

data InputEvent -- {{{2
  = InputReturn
  | InputUp
  | InputDown 
  | InputEscape

data CommandPrefix -- {{{2
  = CmdPrBreakAdd
  | CmdPrBreakList
  | CmdPrContinue
  | CmdPrHelp
  | CmdPrInterrupt
  | CmdPrQuit
  | CmdPrScroll
  | CmdPrStart

data Command -- {{{2
  = CmdUnknown String
  | CmdBreakAdd C.PRow [C.ThreadId]
  | CmdBreakList
  | CmdContinue
  | CmdHelp (Maybe CommandPrefix)
  | CmdInterrupt
  | CmdQuit
  | CmdScroll RowType Int
  | CmdStart

data RowType -- {{{2
  = Tcode | Pcode | Ecode

data Log = Log LogType [String] -- {{{2

data LogType -- {{{2
  = LogOutput
  | LogError
  | LogStatus
  | LogPrompt

type InfoUpdate = [InfoInstance] -> [InfoInstance] -- {{{2

type Fire a = a -> IO () -- {{{2

-- semantics {{{1
instance Read CommandPrefix where -- {{{2
  readsPrec _ command = case filter ((command `isPrefixOf`) . fst) commands of
    [(_, cmd)] -> [(cmd, "")]
    _ -> []

commands :: [(String, CommandPrefix)] -- {{{2
commands = [
    ("badd", CmdPrBreakAdd)
  , ("blist", CmdPrBreakList)
  , ("continue", CmdPrContinue)
  , ("help", CmdPrHelp)
  , ("interrupt", CmdPrInterrupt)
  , ("quit", CmdPrQuit)
  , ("scroll", CmdPrScroll)
  , ("start", CmdPrStart)
  ]

instance Read Command where -- {{{2
  readsPrec _ command = [(parseCommand command, "")]

parseCommand :: String -> Command -- {{{2
parseCommand text =
  let (command:options) = words text in
  case mread command of
    Nothing -> CmdUnknown command
    Just cmd -> case cmd of
      CmdPrBreakAdd -> case options of
        (row@(mread -> Just (_ :: Int)):tids) ->
          let row' = (fromJust . mread) row in
          case sequence (map mread tids) of
            Nothing -> CmdHelp (Just CmdPrBreakAdd)
            Just [] -> CmdBreakAdd row' []
            Just tids' -> CmdBreakAdd row' tids'
        _ -> CmdHelp (Just CmdPrBreakAdd)

      CmdPrBreakList -> noopt CmdPrBreakList CmdBreakList options
      
      CmdPrContinue -> noopt CmdPrContinue CmdContinue options

      CmdPrHelp -> case options of
        [cmd'] -> case mread cmd' of
          Nothing -> CmdUnknown cmd'
          Just cmd'' -> CmdHelp (Just cmd'')
        _ -> CmdHelp Nothing

      CmdPrInterrupt -> noopt CmdPrInterrupt CmdInterrupt options

      CmdPrQuit -> noopt CmdPrQuit CmdQuit options

      CmdPrScroll -> case options of
        [row@(mread -> Just (_ :: Int))] ->
          CmdScroll Pcode ((fromJust . mread) row)

        [rtype@((`elem`["t","p","e"]) -> True), row@(mread -> Just (_ :: Int))] ->
          CmdScroll (read rtype)  ((fromJust . mread) row)

        _ -> CmdHelp (Just CmdPrScroll)

      CmdPrStart -> noopt CmdPrStart CmdStart options

  where
    noopt _   ev [] = ev
    noopt cmd _  _  = CmdHelp (Just cmd)

    mread x = listToMaybe [y | (y,"") <- reads x] 

help :: CommandPrefix -> [String] -- {{{2
help CmdPrBreakAdd  = ["badd prow [tid,...]: add a breakpoint, optionally filtered by thread id"]
help CmdPrBreakList = ["blist: list all breakpoints"]
help CmdPrContinue  = ["continue: continue execution"]
help CmdPrHelp      = ["help: show the list of available commands"]
help CmdPrInterrupt = ["interrupt: interrupt execution"]
help CmdPrQuit      = ["quit: quit ruab"]
help CmdPrScroll    = ["scroll [t|p|e] row: scroll views to the given row. Default row type is p-code."]
help CmdPrStart     = ["start: start execution"]

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
createNetwork :: Context -> Options -> IO () -- {{{2
createNetwork ctx@(Context gui core) opt = do
  let infos = foldr (flip setBreakpoint 0) [] $ C.possible_breakpoints core
  aInfo <- new_actor infos
  let
    fInfo u = update aInfo (\s -> do
        let s' = u s
        renderInfo ctx s'
        return s'
      )

  let fLog      = log (guiLog gui)
  let fResponse = handleResponse fInfo fLog
  let fStatus   = handleStatus ctx fInfo
  fCore        <- C.create_network core opt fResponse fStatus
  let fCommand  = handleCommand core fInfo fLog fCore

  aInput <- new_actor (InputState [] [])
  let fInput = update aInput . handleInput (guiInput gui) fLog fCommand

  _ <- onDestroy (guiWin gui) (fCommand CmdQuit)
  _ <- onKeyPress (guiInput gui) (handleKeyEvent fInput)

  fInfo id
  fLog (Log LogOutput ["welcome"])

renderInfo :: Context -> [InfoInstance] -> IO () -- {{{2
renderInfo (Context gui core) infos = do
  render (guiPcomp gui) infos
  render (guiTcomp gui) (sync p2t infos)
  render (guiEcomp gui) (sync p2e infos)
  where
    render comp infos' = postGUIAsync $ do
      setText (compInfo comp) (render_info infos')
      maybe (return ()) (scrollToRow comp . fst) $ find (infoIsHighlight . snd) infos'

    sync f = map (first ($fromJust_s . f))

    p2t = C.p2t_row core

    p2e prow = case C.p2e_row core prow of
      C.NoMatch -> Nothing
      C.NonCritical erow -> Just erow
      C.Critical ts -> (Just . snd . head) ts

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

handleResponse :: Fire InfoUpdate -> Fire Log -> C.Response -> IO () -- {{{2
handleResponse fInfo fLog = either (fLog . Log LogError . (:[])) handle . snd
  where
    handle (C.ResAddBreakpoint bp) = do
      fLog $ Log LogOutput ["breakpoint added", show bp]
      fInfo $ setBreakpoint (C.breakpointRow bp) (C.breakpointNumber bp)

    handle C.ResContinue = fLog $ Log LogOutput ["continued"]

    handle C.ResInterrupt = fLog $ Log LogOutput ["interrupted"]

    handle (C.ResListBreakpoints bps) = fLog $ Log LogOutput $ "breakpoints:" : map show bps

    handle C.ResShutdown = mainQuit

    handle C.ResStart = fLog $ Log LogOutput ["started"]

handleStatus :: Context -> Fire InfoUpdate -> C.Status -> IO () -- {{{2
handleStatus ctx fInfo status =
  let
    threads = C.statusThreads status
    updateInfo = foldr updateThread id threads
    updateThread thread f = case C.thProw thread of
      Nothing -> f
      Just prow -> f . setThread prow (C.thId thread)
  in do
    fInfo updateInfo
    renderStatus ctx status

renderStatus :: Context -> C.Status -> IO () -- {{{2
renderStatus ctx status = postGUIAsync $
  setText ((guiView . ctxGUI) ctx) $ intercalate "\n" $ (show . C.statusExecution) status : (map show $ C.statusThreads status)

handleCommand :: C.Context -> Fire InfoUpdate -> Fire Log -> Fire C.Command -> Command -> IO () -- {{{2
handleCommand core fInfo fLog fCommand = handle
  where
    handle (CmdUnknown cmd) = fLog $ Log LogError ["unknown command '" ++ cmd ++ "'", "type 'help' for assistance"]

    handle (CmdBreakAdd prow tids) = fCommand $ C.CmdAddBreakpoint prow tids

    handle CmdBreakList = fCommand $ C.CmdListBreakpoints

    handle CmdContinue = fCommand C.CmdContinue

    handle CmdInterrupt = fCommand C.CmdInterrupt

    handle CmdQuit = fCommand C.CmdShutdown

    handle (CmdHelp Nothing) = fLog $ Log LogOutput $
         "available commands:"
        : intercalate ", " (map fst commands)
        : "type 'help <command>' for more information about <command>"
        : []

    handle (CmdHelp (Just cmd)) = fLog $ Log LogOutput (help cmd)

    handle (CmdScroll rt srow) =
      let
        f = case rt of     
          Tcode -> C.t2p_row core
          Pcode -> Just
          Ecode -> C.e2p_row core
      in
        case f srow of
          Nothing -> fLog $ Log LogError ["invalid row number"]
          Just prow -> 
            let
              mtrow = C.p2t_row core prow
              erows = C.p2e_row core prow

              scroll etxt = case mtrow of
                Nothing -> fLog $ Log LogError ["failed to map to t-row"]
                Just trow -> do
                  fLog $ Log LogOutput [
                      "T: " ++ show trow
                    , "P: " ++ show prow
                    , "E: " ++ etxt
                    ]
                  fInfo $ setHighlight prow
            in case erows of
              C.NoMatch -> fLog $ Log LogError ["failed to map to e-rows"]
              C.NonCritical erow -> scroll (show erow)
              C.Critical ts -> scroll (show ts)

    handle CmdStart = fCommand C.CmdStart

handleKeyEvent :: Fire InputEvent -> Event -> IO Bool -- {{{2
handleKeyEvent fInput (Key _ _ _ [] _ _ _ _ "Return" _) = fInput InputReturn >> return True
handleKeyEvent fInput (Key _ _ _ [] _ _ _ _ "Up"     _) = fInput InputUp     >> return True
handleKeyEvent fInput (Key _ _ _ [] _ _ _ _ "Down"   _) = fInput InputDown   >> return True
handleKeyEvent fInput (Key _ _ _ [] _ _ _ _ "Escape" _) = fInput InputEscape >> return True
handleKeyEvent _ _ = return False

handleInput :: Entry -> Fire Log -> Fire Command -> InputEvent -> InputState -> IO InputState -- {{{2
handleInput entry fLog fCommand event state = handle event
  where
  handle InputReturn = do
    command <- entryGetText entry
    if (command /= "")
      then do
        fLog $ Log LogPrompt [command]
        fCommand $ read command
        entrySetText entry ""
        return $ state {
            inputHistory = (command : reverse (inputFuture state) ++ inputHistory state)
          , inputFuture = []
          }
      else
        return state
    
  handle InputUp = case inputHistory state of
    [] -> return state
    (command:history) -> do
      entrySetText entry command
      return $ state {
          inputHistory = history
        , inputFuture = command : inputFuture state
        }

  handle InputDown = case inputFuture state of
    [] -> return state
    [current] -> do
      entrySetText entry ""
      return $ state {
          inputHistory = current : (inputHistory state)
        , inputFuture = []
        }
    (current:next:future) -> do
      entrySetText entry next
      return $ state {
          inputFuture = next : future
        , inputHistory = current : inputHistory state
        }

  handle InputEscape = do
    entrySetText entry ""
    return $ state {
        inputHistory = reverse (inputFuture state) ++ inputHistory state
      , inputFuture = []
      }

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


