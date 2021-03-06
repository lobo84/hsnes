
import qualified Data.ByteString.Lazy as L

import Rom as R
import Cpu as C
import Ppu as P
import Mem as M
import NesMonad
import Numeric
import Text.Printf
import Data.Char
import System.IO
import Data.List (find, intersperse)
import qualified Data.Stream as S
import Control.Applicative
import Control.Monad.State
import Options
import System.Console.ANSI


data Mode = Run
          | CompareLog
          | RomInfo
          | RunState
    deriving (Bounded, Enum, Show)

data MainOptions = MainOptions
    { optMode  :: Mode
    , optRom   :: String
    , optLog   :: String
    , optDebug :: Bool
    }

enumOption name def desc = defineOption (optionType_enum name) (\o -> o
            { optionLongFlags = [name], optionDefault = def, optionDescription = desc })

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> enumOption "mode" Run
            (unwords (intersperse "|" (show `map` (enumFrom Run))))
        <*> simpleOption "rom" "nestest.nes"
            "Path to nes rom"
        <*> simpleOption "log" "nestest.log"
            "Path to log in nestest format"
        <*> simpleOption "debug" False
            "Enable debug trace"

main :: IO ()
main = runCommand $ \opts args -> do

  case optMode opts of
    Run        -> runIndefinite      opts
    CompareLog -> runCompareLog      opts
    RomInfo    -> showRomInfo        opts
    RunState   -> runIndefiniteState opts

showRomInfo :: MainOptions -> IO ()
showRomInfo opts = do
  bytes <- L.readFile (optRom opts)
  case R.parse(bytes) of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom   -> putStrLn $ show rom

runCompareLog :: MainOptions -> IO ()
runCompareLog opts = do
  log   <- readFile   (optLog opts)
  bytes <- L.readFile (optRom opts)
  case R.parse(bytes) of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom   -> putStr ((compareLogs actualLog nesLog))
      where actualLog = S.map toStateStr cpuStream
            cpuStream = runCpuRom opts rom
            nesLog = S.fromList (lines log)

runIndefinite :: MainOptions -> IO ()
runIndefinite opts = do
  bytes <- L.readFile (optRom opts)
  case R.parse bytes of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom   -> do
      cpu <- runIndefiniteFrom romCpu
      return ()
        where romCpu = cpuFromRom opts rom

runIndefiniteFrom :: Cpu -> IO Cpu
runIndefiniteFrom cpu = do
  cpu' <- reactToCpu cpu
  runIndefiniteFrom (C.stepCpu cpu')

reactToCpu :: Cpu -> IO Cpu
reactToCpu cpu = do
      mChar <- stdin `ifReadyDo` getChar
      let cpu' = case mChar of
                   Just x -> resetCpu cpu
                   _        -> cpu
      printTextFrom 0x6004 cpu'
      return cpu'

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

printTextFrom :: Int -> Cpu -> IO ()
printTextFrom addr cpu = do
  if debugTestStatus cpu == 0xDE && ((C.valueAt 0x6000 cpu) /= 0x80)
  then do
    let stat = (printf "%02X %02X %02X %02X"
                  (C.valueAt 0x6000 cpu)
                  (C.valueAt 0x6001 cpu)
                  (C.valueAt 0x6002 cpu)
                  (C.valueAt 0x6003 cpu))
    let line = stat ++ " " ++ (unwords $ lines text)
    hideCursor
    setCursorColumn 0
    putStr line
    clearFromCursorToLineEnd
    showCursor
  else return ()
    where text = (C.textAt addr cpu)
          lineCount = length $ lines text

compareLogs :: S.Stream String -> S.Stream String -> String
compareLogs actual expected = pretty foundResult
  where foundResult = sFind (\(line,(a,e)) -> not (a == e)) linePairs
        linePairs = S.zip lineNumbers (S.zip actual expected)
        lineNumbers = S.fromList [1..]
        pretty (line, diffPair) = (show line) ++ "\n" ++ prettyDiff diffPair

sFind :: Eq a => (a -> Bool) -> S.Stream a -> a
sFind p s = S.head (S.filter p s)

prettyDiff :: (String, String) -> String
prettyDiff (l1, l2) = unlines (map show pairs)
  where pairs = (words l1) `zip` (words l2)

runCpuRom :: MainOptions -> Rom -> S.Stream Cpu
runCpuRom opts rom = cpuStreamFrom (cpuFromRom opts rom)

cpuStreamFrom :: Cpu -> S.Stream Cpu
cpuStreamFrom cpu = S.iterate C.stepCpu cpu

cpuFromRom :: MainOptions -> Rom -> Cpu
cpuFromRom opts rom = (C.initCpu [] mem (optDebug opts))
  where intRom = map (\v -> fromIntegral(v)::Int) (L.unpack(R.prgData rom))
        prgSize = R.sizePrgRom $ R.header rom
        bank1 = zip [0x8000..] intRom
        bank2 = if prgSize == 16 * 1024
                then zip [0xc000..0xFFFF] intRom
                else []
        mem = bank1 ++ bank2

toStateStr :: Cpu -> String
toStateStr cpu = pcv ++ " "++ sep ++
                 "A:" ++ av ++ sep ++
                 "X:" ++ xv ++ sep ++
                 "Y:" ++ yv ++ sep ++
                 "P:" ++ pv ++ sep ++
                 "SP:" ++ spv ++ sep ++
                 "CYC:" ++ cycv
  where regs = (C.registers cpu)
        pcv = map toUpper (showHex16 (C.pc regs))
        sep = " "
        av = hregValueToStr (C.acc regs)
        xv = hregValueToStr (C.x regs)
        yv = hregValueToStr (C.y regs)
        pv = hregValueToStr (C.status regs)
        spv = hregValueToStr (C.sp regs)
        cycv = show ((C.cyc cpu*3) `mod` 341)
        regValueToStr v = printf "%02d" v
        showHex16 v = printf "%04x" v
        hregValueToStr v = map toUpper (printf "%02x" v)

romInfo :: R.Rom -> String
romInfo rom = "Program size = " ++ prgSize ++ "\n" ++ "Rom data = " ++ romData ++ "\n"
  where prgSize = show (R.sizePrgRom (R.header rom))
        romData = show (R.prgData rom)

initC :: MainOptions -> NesState ()
initC opts = do
  nes <- get
  bytes <- lift (L.readFile (optRom opts))
  case R.parse bytes of
    Left errMsg -> lift(putStr ("Parse failed:\n" ++ errMsg))
    Right rom   -> do
      put (nes { cpu = cpuFromRom opts rom })

--  bytes <- L.readFile (optRom opts)
--  case R.parse bytes of
--    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
--    Right rom   -> do
--      cpu <- runIndefiniteFrom romCpu
--      return ()
--        where romCpu = cpuFromRom opts rom

initP :: NesState ()
initP = state $ \nes -> ((), nes { ppu = initPpu (initPpuMem []) })

initNes :: MainOptions -> NesState ()
initNes opts = do
  initC opts
  initP

runNes :: MainOptions -> NesState ()
runNes opts = do
  initNes opts
  fcpu <- runIndefiniteFromState
  return ()


startState = Nes (initCpu [] [] False) (initPpu (initPpuMem [])) -- Empty start state

runIndefiniteState :: MainOptions -> IO ()
runIndefiniteState opts = (runStateT (runNes opts) startState) >>= print

-- runIndefiniteState opts = do
--   cpu <- runIndefiniteFrom (cpu nes)
--   return ()
--     where nes = snd (runStateT (initNes opts) startState)

-- runIndefinite :: MainOptions -> IO ()
-- runIndefinite opts = do
--   bytes <- L.readFile (optRom opts)
--   case R.parse bytes of
--     Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
--     Right rom   -> do
--       cpu <- runIndefiniteFrom romCpu
--       return ()
--         where romCpu = cpuFromRom opts rom
-- 
runIndefiniteFromState :: NesState ()
runIndefiniteFromState = do
  reactToCpuState
  C.stepCpuState
  runIndefiniteFromState

 
reactToCpuState :: NesState ()
reactToCpuState = do
  nes <- get
  mChar <- lift (stdin `ifReadyDo` getChar)
  let cpu' = case mChar of
               Just x -> resetCpu (cpu nes)
               _      -> (cpu nes)
  lift (printTextFrom 0x6004 cpu')
  put (nes { cpu = cpu' })
 
