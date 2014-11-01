import qualified Data.ByteString.Lazy as L

import Rom as R
import Cpu as C
import Numeric
import Text.Printf
import Data.Char
import System.IO
import Data.List

main :: IO ()
main = do
  log <- readFile "nestest.log"
  bytes <- L.readFile "nestest.nes"
  case R.parse(bytes) of
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom -> putStr (show (compareLogs actualLog nesLog) ++ "\n")
      where actualLog = reverse (runCpuRom rom)
            nesLog = (lines log)
      --putStr ("Parse success:\n" ++ (romInfo rom))

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (\x -> isSpace x || x == '\n')

compareLogs actual expected = foundResult
  where foundResult = find (\(line,(a,e)) -> not (a == e)) linePairs
        linePairs = zip [1..] (zip actual expected)

runCpuRom :: Rom -> [String]
runCpuRom rom = str
  where intRom = map (\v -> fromIntegral(v)::Int) (L.unpack(R.prgData rom))
        mem = zip [0xc000..] intRom
        startcpu = (C.initCpu 0xC000 [] mem)
        str = snd (runCpuCyclesState 4000 (startcpu,[]))

runCpuCyclesState :: Int -> (Cpu,[String]) -> (Cpu,[String])
runCpuCyclesState 0 (cpu,ss) = (cpu,ss)
runCpuCyclesState c (cpu,ss) = runCpuCyclesState (c-1) (nextCpu,stateStr)
  where nextCpu = C.stepCpu cpu
        stateStr = (toStateStr cpu):ss


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
