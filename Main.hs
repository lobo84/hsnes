
import qualified Data.ByteString.Lazy as L

import Rom as R
import Cpu as C
import Numeric
import Text.Printf
import Data.Char
import System.IO
import Data.List (find)
import qualified Data.Stream as S
import Data.Stream ((<:>))
import Control.Applicative
import Options


data Mode = Run
          | CompareLog
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
            "Run: run rom, CompareLog: run rom and compare each step against log file"
        <*> simpleOption "rom" "nestest.nes"
            "Path to nes rom"
        <*> simpleOption "log" "nestest.log"
            "Path to log in nestest format"
        <*> simpleOption "debug" False
            "Enable debug trace"


main :: IO ()
main = runCommand $ \opts args -> do

  case optMode opts of
    Run        -> runIndefinite opts
    CompareLog -> runCompareLog opts



runCompareLog :: MainOptions -> IO ()
runCompareLog opts = do
  log   <- readFile   (optLog opts)
  bytes <- L.readFile (optRom opts)
  case R.parse(bytes) of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom -> putStr ((compareLogs actualLog nesLog))
      where actualLog = (runCpuRom opts rom)
            nesLog = S.fromList (lines log)

runIndefinite :: MainOptions -> IO ()
runIndefinite opts = do
  log   <- readFile   (optLog opts)
  bytes <- L.readFile (optRom opts)
  case R.parse(bytes) of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom -> mapM_ putStrLn (S.toList (runCpuRom opts rom))



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (\x -> isSpace x || x == '\n')

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

runCpuRom :: MainOptions -> Rom -> S.Stream String
runCpuRom opts rom = msgStream
  where intRom = map (\v -> fromIntegral(v)::Int) (L.unpack(R.prgData rom))
        mem = zip [0xc000..] intRom
        startcpu = (C.initCpu 0xC000 [] mem (optDebug opts))
        cpuStream = S.iterate C.stepCpu startcpu
        msgStream = S.map toStateStr cpuStream


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
