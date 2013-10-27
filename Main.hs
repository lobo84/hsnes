import qualified Data.ByteString.Lazy as L
import Rom as R

main :: IO ()
main = do 
  bytes <- L.readFile "megaman.nes"
  case R.parse(bytes) of 
    Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
    Right rom -> putStr ("Parse success:\n" ++ (romInfo rom))
                        
  
romInfo :: R.Rom -> String                        
romInfo rom = "Program size = " ++ prgSize  
  where prgSize = show (R.sizePrgRom (R.header rom))