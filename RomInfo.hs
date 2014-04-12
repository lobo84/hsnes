import qualified Data.ByteString.Lazy as L

import Rom as Rom


import System.Environment
  
main :: IO ()
main = do   
  args <- getArgs
  progName <- getProgName
  if ((length args) == 1) 
    then do 
    bytes <- L.readFile (head args)
    case Rom.parse(bytes) of 
      Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
      Right rom -> putStrLn (show rom)
    return ()
    else do 
    putStrLn ("Usage: " ++ progName ++ " " ++ "romfile")
    return()
