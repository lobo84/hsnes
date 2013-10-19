import qualified Data.ByteString.Lazy as L
import Rom as R

main :: IO ()
main = do 
  bytes <- L.readFile "megaman.nes"  
  print (R.parse bytes)
