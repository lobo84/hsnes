module Ppu where

import Debug.Trace
import Data.List
import Data.Bits
import qualified Data.Map as Map
import Data.Word
import Mem as Mem
import qualified Data.Map as M
class ToAdress a where
    toAdress :: a -> Word16

type PpuMemory = Memory Word16 Word8
data Ppu = Ppu {
  registers :: Registers,
  memory :: PpuMemory,
  cycle :: Int,
  display :: Display
}

displayWidth = 256
displayHeight = 240



type PixelValue = Int
type Display = M.Map (Int,Int) PixelValue


initDisplay :: Display
initDisplay = M.empty
        
type RegValue = Int

data RegisterType = Ctrl |
                    Mask |
                    Status |
                    OamAddr |
                    OamData |
                    Scroll |
                    Address |
                    Pdata

data Registers = Registers {
  ctrl :: RegValue,
  mask :: RegValue,
  status :: RegValue,
  oamAddr :: RegValue,
  oamData :: RegValue,
  scroll :: RegValue,
  address :: RegValue,
  pdata :: RegValue
}

regMap Ctrl = 0x2000
regMap Mask = 0x2001
regMap Status = 0x2002
regMap OamAddr = 0x2003
regMap OamData = 0x2004
regMap Scroll = 0x2005
regMap Address = 0x2006
regMap Pdata = 0x2007

updateRegister :: RegisterType -> RegValue -> Registers -> Registers
updateRegister Ctrl value regs = regs{ctrl=value}
updateRegister Mask value regs = regs{mask=value}
updateRegister Status value regs = regs{status=value}
updateRegister OamAddr value regs = regs{oamAddr=value}
updateRegister OamData value regs = regs{oamData=value}
updateRegister Scroll value regs = regs{scroll=value}
updateRegister Address value regs = regs{address=value}
updateRegister Pdata value regs = regs{pdata=value}

updateRegisters :: [(RegisterType,RegValue)] -> Registers -> Registers
updateRegisters ((r,v):vs) regValue = updateRegisters vs (updateRegister r v regValue)
updateRegisters [] r = r

readRegister :: RegisterType -> Registers -> RegValue
readRegister Ctrl regs = ctrl(regs)
readRegister Mask regs = mask(regs)
readRegister Status regs = status(regs)
readRegister OamAddr regs = oamAddr(regs)
readRegister OamData regs = oamData(regs)
readRegister Scroll regs = scroll(regs)
readRegister Address regs = address(regs)
readRegister Pdata regs = pdata(regs)

regs = [Ctrl, Mask, Status, OamAddr, OamData, Scroll, Address, Pdata]

regsAddr :: [(RegisterType, Int)]
regsAddr = map (\r -> (r,regMap r)) regs

regsFromMem :: Memory Int Int -> Registers
regsFromMem mem = updateRegisters regList initRegisters
  where regList = map (\x -> (x,readMem (regMap x) mem )) regs
  
regsToMem :: Registers -> Memory Int Int -> Memory Int Int
regsToMem rs mem = foldr (\(reg,addr) -> writeMem addr (readRegister reg rs)) mem regsAddr

updatePpuRegs :: Ppu -> Memory Int Int -> Ppu
updatePpuRegs ppu mem = ppu{registers = (regsFromMem mem)}

updateMemory :: Ppu -> Memory Int Int -> Memory Int Int
updateMemory ppu mem = regsToMem (registers ppu) mem

data VramAddrInc = 
      Add1Across
    | Add32Down
    deriving (Show, Eq)


data NameTblBase = 
      X2000
    | X2400
    | X2800
    | X2C00
    deriving (Show, Eq)

instance ToAdress NameTblBase where
    toAdress X2000 = 0x2000
    toAdress X2400 = 0x2400
    toAdress X2800 = 0x2800
    toAdress X2C00 = 0x2C00

data PatTblAddr = 
      X0000
    | X1000
    deriving (Show, Eq)

instance ToAdress PatTblAddr where
    toAdress X0000 = 0x0000
    toAdress X1000 = 0x1000

data SpriteSize = 
      Size8x8
    | Size8x16
    deriving (Show, Eq)

data PpuMasterSlave = 
      ReadBackdrop
    | OutputColor
    deriving (Show, Eq)

data PPUCTRL = PPUCTRL {
  generateNMI      :: Bool,           -- Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
  ppuMasterSlave   :: PpuMasterSlave, -- PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins)
  spriteSize       :: SpriteSize,     -- Sprite size (0: 8x8; 1: 8x16)
  bgPatTblAddr     :: PatTblAddr,     -- Background pattern table address (0: $0000; 1: $1000)
  spritePatTblAddr :: PatTblAddr,     -- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
  vramAddrInc      :: VramAddrInc,    -- RAM address increment per CPU read/write of PPUDATA, (0: add 1, going across; 1: add 32, going down)
  nameTblBase      :: NameTblBase     -- Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
} deriving (Show, Eq)

data PPUMASK = PPUMASK {
    intensifyBlues      :: Bool,  -- Intensify blues (and darken other colors)
    intensifyGreens     :: Bool, -- Intensify greens (and darken other colors)
    intensifyReds       :: Bool, -- Intensify reds (and darken other colors)
    showSprites         :: Bool, -- 1: Show sprites
    showBg              :: Bool, -- 1: Show background
    showLeftmostSprites :: Bool, -- 1: Show sprites in leftmost 8 pixels of screen; 0: Hide
    showLeftmostBg      :: Bool, -- 1: Show background in leftmost 8 pixels of screen; 0: Hide
    grayscale           :: Bool -- Grayscale (0: normal color; 1: produce a monochrome display)
} deriving (Show, Eq)

data PPUSTATUS = PPUSTATUS {
    vblankStarted :: Bool,  -- Vertical blank has started (0: not in VBLANK; 1: in VBLANK).
                            -- Set at dot 1 of line 241 (the line *after* the post-render
                            -- line); cleared after reading $2002 and at dot 1 of the
                            -- pre-render line.
    spriteZeroHit :: Bool,  -- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
                            -- a nonzero background pixel; cleared at dot 1 of the pre-render
                            -- line.  Used for raster timing.
    spriteOverflow :: Bool -- Sprite overflow. The intent was for this flag to be set
                            -- whenever more than eight sprites appear on a scanline, but a
                            -- hardware bug causes the actual behavior to be more complicated
                            -- and generate false positives as well as false negatives; see
                            -- PPU sprite evaluation. This flag is set during sprite
                            -- evaluation and cleared at dot 1 (the second dot) of the
                            -- pre-render line.
} deriving (Show, Eq)

bitToValue :: Bits b => b -> Int -> (a, a) -> a
bitToValue reg bit (f, t) = case (testBit reg bit) of
                              False -> f
                              True  -> t

parsePPUCTRL :: RegValue -> PPUCTRL
parsePPUCTRL d = PPUCTRL generateNMI ppuMasterSlave spriteSize bgPatTblAddr spritePatTblAddr vramAddrInc nameTblBase
  where
    generateNMI      = bitToValue d 7 (False, True)
    ppuMasterSlave   = bitToValue d 6 (ReadBackdrop, OutputColor)
    spriteSize       = bitToValue d 5 (Size8x8, Size8x16)
    bgPatTblAddr     = bitToValue d 4 (X0000, X1000)
    spritePatTblAddr = bitToValue d 3 (X0000, X1000)
    vramAddrInc      = bitToValue d 2 (Add1Across, Add32Down)
    nameTblBase      = case (testBit d 1, testBit d 0) of
                   (False, False) -> X2000
                   (False, True)  -> X2400
                   (True,  False) -> X2800
                   (True,  True)  -> X2C00

parsePPUMASK :: RegValue -> PPUMASK
parsePPUMASK d = PPUMASK intensifyBlues intensifyGreens intensifyReds showSprites showBg showLeftmostSprites showLeftmostBg grayscale
  where
    intensifyBlues       = (testBit d 7)
    intensifyGreens      = (testBit d 6)
    intensifyReds        = (testBit d 5)
    showSprites          = (testBit d 4)
    showBg               = (testBit d 3)
    showLeftmostSprites  = (testBit d 2)
    showLeftmostBg       = (testBit d 1)
    grayscale            = (testBit d 0)

parsePPUSTATUS :: RegValue -> PPUSTATUS
parsePPUSTATUS d = PPUSTATUS vblankStarted spriteZeroHit spriteOverflow
  where
    vblankStarted  = (testBit d 7)
    spriteZeroHit  = (testBit d 6)
    spriteOverflow = (testBit d 5)

nametableSize = 960
attributeTableSize = 64
type Nametable = [Word8]
type Attrtable = [Word8]

nametableFetch :: Ppu -> Nametable
nametableFetch ppu = Mem.readMemRange nametable nametableEnd (memory ppu)
  where nametable = nametableBase ppu
        nametableEnd = nametable + nametableSize

attributeTableFetch :: Ppu -> Attrtable
attributeTableFetch ppu = Mem.readMemRange attributeTable attributeTableEnd (memory ppu)
  where attributeTable = (nametableBase ppu) + nametableEnd + 1
        attributeTableEnd = attributeTable + attributeTableSize
        nametableEnd = (nametableBase ppu) + nametableSize

to16BitAddress x = (fromIntegral x)::Word16

colorToPixel :: PatternColor -> Int
colorToPixel Color1 = 0
colorToPixel Color2 = 255
colorToPixel Color3 = 300
colorToPixel Color4 = 100

data PatternColor = Color1 | Color2 | Color3 | Color4
    deriving (Show, Eq)
type Pattern = [[PatternColor]]
type Address = Word16


patternToFlatList :: Pattern -> [PatternColor]
patternToFlatList ps = concat ps

patternTableSize = 0x1FFF+1

patternTableFetchMem :: PpuMemory -> PatTblAddr -> [Pattern]
patternTableFetchMem mem patTbl = combinedPatterns
  where patternBase = (toAdress patTbl)
        addresses = takeWhile (\x -> x < patternTableSize) [patternBase+m*16 | m <- [0..]]
        patterns = map (\a -> fetchPattern mem a) addresses 
        combinedPatterns = map combinePattern patterns


patternTableFetch :: PpuMemory -> PatTblAddr -> Nametable -> [Pattern]
patternTableFetch mem patTbl indexes = combinedPatterns
  where patternBase = (toAdress patTbl)
        addresses = map (\i -> (to16BitAddress i)+patternBase) indexes
        patterns = map (\a -> fetchPattern mem a) addresses 
        combinedPatterns = map combinePattern patterns
        
update :: Coord -> PixelValue -> Display -> Display
update n new d = M.insert n new d

type Coord = (Int,Int)
setPixel :: Coord -> PixelValue -> Display -> Display
setPixel (x,y) value display = update (x,y) value display

imgIndex :: Coord -> Int
imgIndex (x,y) = (y*displayWidth + x)


type Image = [Int]

patternToImage :: Pattern -> Image
patternToImage p = map colorToPixel (patternToFlatList p)

updateDisplay :: [(Coord,PixelValue)] -> Display -> Display
updateDisplay ((c,pv):vs) d = updateDisplay vs (setPixel c pv d)
updateDisplay [] d = d

drawPattern :: Pattern -> Coord -> Display -> Display        
drawPattern pattern (nx,ny) d = updateDisplay toUpdate d
 where img = patternToImage pattern
       imgWidth = 8
       imgHeight = 8
       imgPixels = img
       toUpdate = zip imgCoords imgPixels
       imgCoords = [(x+nx,y+ny) | y <- [0..imgHeight-1], x <- [0..imgWidth-1]]


drawPatterns :: [Pattern] -> Display        
drawPatterns ps = foldr (\(p,(x,y)) -> drawPattern p (x,y)) initDisplay sprites
  where sprites = zip ps coords
        coords = [(x*8,y*8) | y <- [0..23] , x <- [0..15]]
  
drawSprites :: Ppu -> Display
drawSprites ppu = drawPatterns (patternTableFetchMem (memory ppu) patTblAdr)
  where patTblAdr = trace (show tblAddr) tblAddr
        tblAddr = (bgPatTblAddr ctrlReg)
        ctrlReg = parsePPUCTRL (ctrl (registers ppu))
  
combinePattern :: ([Word8], [Word8]) -> Pattern        
combinePattern (p1,p2) = map (\x -> toPatternColors (fst x) (snd x)) (zip p1 p2)

toPatternColors :: Word8 -> Word8 -> [PatternColor]
toPatternColors p1 p2 = map toPatternColor (zip (toBits p1) (toBits p2))
  where toBits x = map (testBit x) (reverse [0..7])
             
toPatternColor :: (Bool,Bool) -> PatternColor
toPatternColor (True,True) = Color1
toPatternColor (False,True) = Color2
toPatternColor (True,False) = Color3
toPatternColor (False,False) = Color4  

fetchPattern :: PpuMemory -> Address -> ([Word8],[Word8])
fetchPattern mem start = (first,second)
  where first = Mem.readMemRange fstStart fstEnd mem
        second = Mem.readMemRange sndStart sndEnd mem
        fstStart = start
        fstEnd = start+7
        sndStart = fstEnd+1
        sndEnd = sndStart+7
        
nametableBase :: Ppu -> Word16
nametableBase ppu = toAdress (nameTblBase (parsePPUCTRL (ctrl (registers ppu))))

stepPpu :: Ppu -> Ppu
stepPpu ppu = ppu

initRegisters :: Registers
initRegisters = Registers {
  ctrl = 0x00,
  mask = 0x0,
  status = 0x0,
  scroll = 0x0,
  address = 0x0,
  pdata = 0x0,
  oamAddr = 0x0,
  oamData = 0x0
  }
                
initPpu :: PpuMemory -> Ppu
initPpu mem = Ppu initRegisters mem 0 initDisplay
