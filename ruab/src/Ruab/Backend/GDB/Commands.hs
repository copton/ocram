module Ruab.Backend.GDB.Commands where

-- imports {{{1
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Ruab.Backend.GDB.Representation

-- types {{{1
type Location = String -- {{{2

positive_offset_location :: Int -> Location -- {{{3
positive_offset_location offset = "+" ++ show offset

negative_offset_location :: Int -> Location -- {{{3
negative_offset_location offset = "-" ++ show offset

file_line_location :: String -> Int -> Location -- {{{3
file_line_location filename linenum = filename ++ ":" ++ show linenum

function_location :: String -> Location -- {{{3
function_location = id

function_label_location :: String -> String -> Location -- {{{3
function_label_location function label = function ++ ":" ++ label

file_function_location :: String -> String -> Location -- {{{3
file_function_location filename function = filename ++ ":" ++ function

label_location :: String -> Location -- {{{3
label_location = id

plain_address_location :: String -> Location -- {{{3
plain_address_location = ("*"++)

expr_address_location :: String -> Location -- {{{3
expr_address_location = plain_address_location

func_address_location :: String -> Location -- {{{3
func_address_location = plain_address_location

file_func_address_location :: String -> String -> Location -- {{{3
file_func_address_location filename funcaddr = "'" ++ filename ++ "'::" ++ funcaddr

data PrintValues -- {{{2
  = NoValues
  | AllValues
  | SimpleValues

instance Show PrintValues where
  show NoValues     = "--no-values"
  show AllValues    = "--all-values"
  show SimpleValues = "--simple-values"

mapPrintValues :: (PrintValues -> a) -> Int -> a
mapPrintValues f 0 = f NoValues
mapPrintValues f 1 = f AllValues
mapPrintValues f 2 = f SimpleValues
mapPrintValues _ _ = error "valid integers for the print-value parameter range from 0 to 2 only"

data FrameSelect -- {{{2
  = FrameAddr String
  | CurrentFrame
  | Floating

instance Show FrameSelect where
  show (FrameAddr addr) = addr
  show CurrentFrame = "*"
  show Floating = "@"

data FormatSpec -- {{{2
  = Binary
  | Decimal
  | Hexadecimal
  | Octal
  | Natural

instance Show FormatSpec where
  show Binary = "binary"
  show Decimal = "decimal"
  show Hexadecimal = "hexadecimal"
  show Octal = "octal"
  show Natural = "natural"

data FrozenFlag -- {{{2
  = Frozen
  | Unfrozen

instance Show FrozenFlag where
  show Frozen = "1"
  show Unfrozen = "0"

data DisassemblyMode -- {{{2
  = DisassemblyMode Bool Bool -- mixed source and disassembly, raw opcodes

instance Show DisassemblyMode where
  show (DisassemblyMode False False) = "0"
  show (DisassemblyMode True False) = "1"
  show (DisassemblyMode False True) = "2"
  show (DisassemblyMode True True) = "3"

data DataFormat -- {{{2
  = DHexadecimal
  | DOctal
  | DBinary
  | DDecimal
  | DRaw
  | DNatural

instance Show DataFormat where
  show DHexadecimal = "x"
  show DOctal = "o"
  show DBinary = "t"
  show DDecimal = "d"
  show DRaw = "r"
  show DNatural = "N"

data OutputFormat -- {{{2
  = HexadecimalInteger
  | SignedDecimalInteger
  | UnsignedDecimalInteger
  | OctalInteger
  | BinaryInteger
  | Address
  | CharacterConstantInteger
  | FloatingPointNumber
  | OString
  | Raw

instance Show OutputFormat where
  show HexadecimalInteger = "x"
  show SignedDecimalInteger = "d"
  show UnsignedDecimalInteger = "u"
  show OctalInteger = "o"
  show BinaryInteger = "t"
  show Address = "a"
  show CharacterConstantInteger = "c"
  show FloatingPointNumber = "f"
  show OString = "s"
  show Raw = "r"

-- helper {{{1
add_token :: Token -> Command -> Command -- {{{2
add_token token (MICommand _ x y z) = MICommand (Just token) x y z
add_token token (CLICommand _ x) = CLICommand (Just token) x

add_parameters :: [Parameter] -> Command -> Command -- {{{2
add_parameters ps (MICommand x y z ps') = MICommand x y z (ps'++ps)
add_parameters ps (CLICommand t s) = CLICommand t (s ++ intercalate " " ps)

-- commands {{{1
-- breakpoint commands {{{2
break_after :: Int -> Int -> Command -- {{{3
break_after number count = cmd "break-after" $ map opt [number, count]

break_commands :: Int -> [String] -> Command -- {{{3
break_commands number commands = cmd "break-commands" $ opt number : map opt commands

break_condition :: Int -> String -> Command -- {{{3
break_condition number expr = cmd "break-condition" $ [opt expr]

break_delete :: [Int] -> Command -- {{{3
break_delete numbers = cmd "break-delete" $ map opt numbers

break_disable :: [Int] -> Command -- {{{3
break_disable numbers = cmd "break-disable" $ map opt numbers

break_enable :: [Int] -> Command -- {{{3
break_enable numbers = cmd "break-enable" $ map opt numbers

break_info :: Int -> Command -- {{{3
break_info number = cmd "break-info" [opt number]

break_insert :: Bool -> Bool -> Bool -> Bool -> Bool -> Maybe String -> Maybe Int -> Maybe Int -> Location -> Command -- {{{3
break_insert temporary hardware pending disabled tracepoint condition ignoreCount threadId location =
  cmd "break-insert" $ catMaybes [temporary', hardware', pending', disabled', tracepoint', condition', ignoreCount', threadId'] ++ [opt location]
  where
    temporary'   = boolOpt "-t" temporary
    hardware'    = boolOpt "-h" hardware
    pending'     = boolOpt "-p" pending
    disabled'    = boolOpt "-d" disabled
    tracepoint'  = boolOpt "-a" tracepoint
    condition'   = maybOpt "-c" condition
    ignoreCount' = maybOpt "-i" ignoreCount
    threadId'    = maybOpt "-p" threadId
    
break_list :: Command -- {{{3
break_list = cmd "break-list" []

break_passcount :: Int -> Int -> Command -- {{{3
break_passcount tracepointNumber passcount = cmd "break-passcount" $ map opt [tracepointNumber, passcount]

break_watch :: Bool -> Command -- {{{3
break_watch access =  cmd "break-watch" [opt (if access then "-a" else "-r")]

-- program context {{{2
exec_arguments :: [String] -> Command -- {{{3
exec_arguments args = cmd "exec-arguments" $ map opt args

environment_cd :: String -> Command -- {{{3
environment_cd pathdir = cmd "environment-cd" [opt pathdir]

environment_directory :: Bool -> [String] -> Command -- {{{3
environment_directory reset pathdirs = cmd "environment-directory" $ catMaybes [boolOpt "-r" reset] ++ map opt pathdirs

environment_path :: Bool -> [String] -> Command -- {{{3
environment_path reset pathdirs = cmd "environment-path" $ catMaybes [boolOpt "-r" reset] ++ map opt pathdirs

environment_pwd :: Command -- {{{3
environment_pwd = cmd "environment-pwd" []

-- thread commands {{{2
thread_info :: Maybe Int -> Command -- {{{3
thread_info threadId = cmd "thread-info" $ catMaybes [fmap opt threadId]

thread_list_ids :: Command -- {{{3
thread_list_ids = cmd "thread-list-ids" []

thread_select :: Int -> Command -- {{{3
thread_select threadnum = cmd "thread-select" [opt threadnum]

-- ada tasking commands -- TODO {{{2

-- program execution {{{2
exec_continue :: Bool -> Either Bool Int -> Command -- {{{3
exec_continue reverse x = cmd "exec-continue" $ catMaybes [reverse', x']
  where
    reverse'     = boolOpt "--reverse" reverse
    x' = case x of
      Left all -> boolOpt "--all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_finish :: Bool -> Command -- {{{3
exec_finish reverse = cmd "exec-finish" $ catMaybes [boolOpt "--reverse" reverse]

exec_interrupt :: Either Bool Int -> Command -- {{{3
exec_interrupt x = cmd "exec-interrupt" $ x' ?: [] 
  where
    x' = case x of
      Left all -> boolOpt "-all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_jump :: Location -> Command -- {{{3
exec_jump location = cmd "exec-jump" [opt location]

exec_next :: Command -- {{{3
exec_next = cmd "exec-next" []

exec_next_instruction :: Bool -> Command -- {{{3
exec_next_instruction reverse = cmd "exec-next-instruction" $ catMaybes [boolOpt "--reverse" reverse]

exec_return :: Command -- {{{3
exec_return = cmd "exec-return" []

exec_run :: Either Bool Int -> Command -- {{{3
exec_run x = cmd "exec-run" $ x' ?: []
  where
    x' = case x of
      Left all -> boolOpt "-all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_step :: Command -- {{{3
exec_step = cmd "exec-step" []

exec_step_instruction :: Bool -> Command -- {{{3
exec_step_instruction reverse = cmd "exec-step-instruction" $ catMaybes [boolOpt "--reverse" reverse]
  
exec_until :: Location -> Command -- {{{3
exec_until location = cmd "exec-until" [opt location]

-- stack manipulation {{{2
stack_info_frame :: Command -- {{{3
stack_info_frame = cmd "stack-info-frame" []

stack_info_depth :: Maybe Int -> Command  -- {{{3
stack_info_depth maxDepth = cmd "stack-info-depth" $ catMaybes [fmap opt maxDepth]

stack_list_arguments :: PrintValues -> Maybe (Int, Int) -> Command -- {{{3
stack_list_arguments printValues frames = cmd "stack-list-arguments" $ opt printValues : maybTupleOpt frames

stack_list_arguments' :: Int -> Maybe (Int, Int) -> Command
stack_list_arguments' = mapPrintValues stack_list_arguments

stack_list_frames :: Maybe (Int, Int) -> Command -- {{{3
stack_list_frames frames = cmd "stack-list-frames" $ maybTupleOpt frames

stack_list_locals :: PrintValues -> Command -- {{{3
stack_list_locals printValues = cmd "stack-list-locals" [opt printValues]

stack_list_locals' :: Int -> Command -- {{{3
stack_list_locals' = mapPrintValues stack_list_locals

stack_list_variables :: PrintValues -> Command -- {{{3
stack_list_variables printValues = cmd "stack-list-variable" [opt printValues]

stack_list_variables' :: Int -> Command -- {{{3
stack_list_variables' = mapPrintValues stack_list_variables

stack_select_frame :: Int -> Command -- {{{3
stack_select_frame framenum = cmd "stack-select-frame" [opt framenum]

-- variable objects {{{2
enable_pretty_printing :: Command -- {{{3
enable_pretty_printing = cmd "enable-pretty-printing" []

var_create :: Maybe String -> FrameSelect -> String -> Command -- {{{3
var_create name frameSelect expression = cmd "var-create" $ [name', opt frameSelect, opt expression]
  where
    name' = opt (fromMaybe "-" name)

var_delete :: Bool -> String -> Command -- {{{3
var_delete children name = cmd "var-delete" $ boolOpt "-c" children ?: opt name : []

var_set_format :: String -> FormatSpec -> Command -- {{{3
var_set_format name formatSpec = cmd "var-set-format" [opt name, opt formatSpec]

var_show_format :: String -> Command -- {{{3
var_show_format name = cmd "var-show-format" [opt name]

var_info_num_children :: String -> Command -- {{{3
var_info_num_children name = cmd "var-info-num-children" [opt name]

var_list_children :: Maybe PrintValues -> String -> Maybe (Int, Int) -> Command -- {{{3
var_list_children Nothing name range = var_list_children (Just NoValues) name range
var_list_children (Just printValues) name range = cmd "var-list-children" $ opt printValues : maybTupleOpt range

var_list_children' :: Int -> String -> Maybe (Int, Int) -> Command
var_list_children' = mapPrintValues (var_list_children . Just)

var_info_type :: Command -- {{{3
var_info_type = cmd "var-info-type" []

var_info_expression :: String -> Command -- {{{3
var_info_expression name = cmd "var-info-expression" [opt name]

var_info_path_expressoin :: String -> Command -- {{{3
var_info_path_expressoin name = cmd "var-info-path-expression" [opt name]

var_show_attributes :: String -> Command -- {{{3
var_show_attributes name = cmd "var-show-attributes" [opt name]

var_evaluate_expression :: Maybe FormatSpec -> String -> Command -- {{{3
var_evaluate_expression formatSpec name = cmd "var-evaluate-expression" $ maybOpt "-f" formatSpec ?: opt name : []

var_assign :: String -> String -> Command -- {{{3
var_assign name expression = cmd "var-assign" [opt name, opt expression]

var_update :: Maybe PrintValues -> Maybe String -> Command -- {{{3
var_update Nothing name = var_update (Just NoValues) name
var_update (Just printValues) name = cmd "var-update" [opt printValues, opt name]

var_set_frozen :: String -> FrozenFlag -> Command -- {{{3
var_set_frozen name flag = cmd "var-set-frozen" [opt name, opt flag]

var_set_update_range :: String -> Int -> Int -> Command -- {{{3
var_set_update_range name from to = cmd "var-set-update-range" [opt name, opt from, opt to]

var_set_visualizer :: String -> String -> Command -- {{{3
var_set_visualizer name visualizer = cmd "ver-set-visualizer" [opt name, opt visualizer]

-- data manipulation {{{2
data_disassemble :: Either (String, String) (String, Int, Maybe Int) -> DisassemblyMode -> Command -- {{{3
data_disassemble x mode = MICommand Nothing "data-disassemble" options [show mode]
  where
    options = case x of
      Left (start, end) -> [opt' "-s" start, opt' "-e" end]
      Right (filename, linenum, lines) -> [opt' "-f" filename, opt' "-l" linenum] ++ catMaybes [maybOpt "-n" lines]

data_evaluate_expression :: String -> Command -- {{{3
data_evaluate_expression expr = cmd "data-evaluate-expression" [opt expr]

data_list_changed_registers :: Command -- {{{3
data_list_changed_registers = cmd "data-list-changed-registers" []

data_list_register_names :: [Int] -> Command -- {{{3
data_list_register_names regnos = cmd "data-list-register-names" $ map opt regnos

data_list_register_values :: DataFormat -> [Int] -> Command -- {{{3
data_list_register_values fmt regnos = cmd "data-list-register-values" $ opt fmt : map opt regnos

data_read_memory :: Maybe Int -> String -> OutputFormat -> Int -> Int -> Int -> Maybe Char -> Command -- {{{3
data_read_memory byteOffset address wordFormat wordSize nrRows nrCols asChar =
  cmd "data-read-memory" $ maybOpt "-o" byteOffset ?: opt address : opt wordFormat : opt nrRows : opt nrCols : fmap opt asChar ?: []

data_read_memory_bytes :: Maybe Int -> String -> Int -> Command -- {{{3
data_read_memory_bytes byteOffset address count = cmd "data-read-memory-bytes" $ maybOpt "-o" byteOffset ?: opt address : opt count : []

data_write_memory_bytes :: String -> String -> Command -- {{{3
data_write_memory_bytes address contents = cmd "data-write-memory-bytes" [opt address, opt contents]

-- tracepoint commands {{{2


-- symbol query {{{2


-- file commands {{{2

-- target manipulation {{{2

-- file transfer commands {{{2

-- miscellaneous commmands {{{2

-- utils {{{1
cmd :: String -> [Option] -> Command -- {{{2
cmd operation options = MICommand Nothing operation options []

opt :: Show a => a -> Option -- {{{2
opt parameter = Option (show parameter) Nothing

opt' :: (Show a, Show b) => a -> b -> Option -- {{{2
opt' name value = Option (show name) (Just (show value))

boolOpt :: String -> Bool -> Maybe Option
boolOpt _ False = Nothing
boolOpt flag True = Just (opt flag)

maybOpt :: Show a => String -> Maybe a -> Maybe Option
maybOpt _ Nothing = Nothing
maybOpt flag param = Just (Option flag (fmap show param))

maybTupleOpt :: Show a => Maybe (a, a) -> [Option]
maybTupleOpt Nothing = []
maybTupleOpt (Just (lowFrame, highFrame)) = map opt [lowFrame, highFrame]


(?:) :: Maybe a -> [a] -> [a] -- {{{1
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:
