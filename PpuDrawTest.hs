import qualified Data.ByteString.Lazy as L

import Rom as Rom
import Ppu as Ppu
import Mem as Mem
import Foreign

import Data.Char
import Data.Word
import Data.Maybe
import Data.List
import Data.Array

import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL
import System.Environment
import qualified Data.Map as M  
--main :: IO ()
--main = do   


screenWidth  = Ppu.displayWidth
screenHeight = Ppu.displayHeight
screenBpp    = 32

getPixel32 :: Int -> Int -> Surface -> IO Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    Pixel `liftM` peekElemOff pixels ((y * surfaceGetWidth s) + x)

putPixel32 :: Int -> Int -> Pixel -> Surface -> IO ()
putPixel32 x y (Pixel pixel) s = do
    pixels <- castPtr `liftM` surfaceGetPixels s
    pokeElemOff pixels ((y * surfaceGetWidth s) + x) pixel

initEnv :: Rom -> IO ()
initEnv rom = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Flip Test" []
    drawPpu rom screen
    Graphics.UI.SDL.flip screen

loop :: IO ()
loop = do
    quit <- whileEvents $ \_ -> return ()    
    unless quit loop

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

drawDisplay :: [((Int,Int),Int)] -> Surface -> IO ()
drawDisplay ps s = sequence_ (map (\((x,y),v) -> putPixel32 x y (Pixel (fromIntegral v)) s) ps)

drawPpu :: Rom -> Surface -> IO ()
drawPpu rom screen = drawDisplay (toPixels rom) screen

main = withInit [InitEverything] $ do -- withInit calls quit for us.
  args <- getArgs
  progName <- getProgName
  if ((length args) == 1) 
    then do 
    bytes <- L.readFile (head args)
    case Rom.parse(bytes) of 
      Left errMsg -> putStr ("Parse failed:\n" ++ errMsg)
      Right rom -> do 
        putStrLn (show rom)
        initEnv rom
--        drawPpu rom
        loop
    return ()
    else do 
    putStrLn ("Usage: " ++ progName ++ " " ++ "romfile")
    return()    


toPixels :: Rom -> [((Int,Int),Int)]
toPixels rom = display
  where ppu = Ppu.initPpu (Mem.initMem (zip [0..] (L.unpack mem)))
        mem = Rom.chrData rom
        display = zip [(x,y) | y <- [0..Ppu.displayHeight-1], x <- [0..Ppu.displayWidth-1]] (Ppu.drawSprites ppu)