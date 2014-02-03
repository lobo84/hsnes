import qualified Data.ByteString.Lazy as L

import Rom as R
import Cpu as C

main :: IO ()
main = do 
  bytes <- L.readFile "nestest.nes"
  case R.parse(bytes) of 
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom -> C.runCpuInteractive (C.initCpu 0 intRom [])
      where intRom = map (\v -> fromIntegral(v)::Int) (L.unpack(R.prgData rom))
      --putStr ("Parse success:\n" ++ (romInfo rom))
                        
  
romInfo :: R.Rom -> String                        
romInfo rom = "Program size = " ++ prgSize ++ "\n" ++ "Rom data = " ++ romData ++ "\n"
  where prgSize = show (R.sizePrgRom (R.header rom))
        romData = show (R.prgData rom)