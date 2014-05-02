module Ppu where


import Data.List
import Data.Bits
import qualified Data.Map as Map
import Data.Word
import Mem as Mem
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
type Display = [PixelValue]


initDisplay :: Display
initDisplay = replicate (displayWidth*displayHeight) 0
        
type RegValue = Word8

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
colorToPixel Color1 = 0x1
colorToPixel Color2 = 0xFF
colorToPixel Color3 = 0xFFFF
colorToPixel Color4 = 0xFFFFFF

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
        addresses = takeWhile (\x -> x < patternTableSize) [m+16 | m <- [0..]]
        patterns = map (\a -> fetchPattern mem a) addresses 
        combinedPatterns = map combinePattern patterns


patternTableFetch :: PpuMemory -> PatTblAddr -> Nametable -> [Pattern]
patternTableFetch mem patTbl indexes = combinedPatterns
  where patternBase = (toAdress patTbl)
        addresses = map (\i -> (to16BitAddress i)+patternBase) indexes
        patterns = map (\a -> fetchPattern mem a) addresses 
        combinedPatterns = map combinePattern patterns
        
update :: Int -> a -> [a] -> [a]        
update n new xs = map (\(x,i) -> if i == n then new else x) (zip xs [0..])

type Coord = (Int,Int)
setPixel :: Coord -> PixelValue -> Display -> Display
setPixel (x,y) value display = update (imgIndex (x,y)) value display

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
drawPatterns ps = p15
  where p1 = drawPattern (ps !! 0) (0,0) initDisplay
        p2 = drawPattern (ps !! 1) (0,10) p1
        p3 = drawPattern (ps !! 2) (0,20) p2
        p4 = drawPattern (ps !! 2) (0,30) p3
        p5 = drawPattern (ps !! 2) (0,40) p4        
        p6 = drawPattern (ps !! 2) (0,50) p5        
        p7 = drawPattern (ps !! 2) (0,60) p6        
        p8 = drawPattern (ps !! 2) (0,70) p7        
        p9 = drawPattern (ps !! 2) (0,80) p8        
        p10 = drawPattern (ps !! 2) (0,90) p9        
        p11 = drawPattern (ps !! 2) (0,100) p10        
        p12 = drawPattern (ps !! 2) (0,110) p11        
        p13 = drawPattern (ps !! 2) (0,120) p12        
        p14 = drawPattern (ps !! 2) (0,130) p13        
        p15 = drawPattern (ps !! 2) (0,140) p14        

drawSprites :: Ppu -> Display
drawSprites ppu = drawPatterns (patternTableFetchMem (memory ppu) patTblAdr)
  where patTblAdr = bgPatTblAddr ctrlReg
        ctrlReg = parsePPUCTRL (ctrl (registers ppu))
  
combinePattern :: ([Word8], [Word8]) -> Pattern        
combinePattern (p1,p2) = map (\x -> toPatternColors (fst x) (snd x)) (zip p1 p2)

toPatternColors :: Word8 -> Word8 -> [PatternColor]
toPatternColors p1 p2 = map toPatternColor (zip (toBits p1) (toBits p2))
  where toBits x = map (testBit x) [0..7]
             
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
  
initPpu :: PpuMemory -> Ppu
initPpu mem = Ppu Registers {
                  ctrl = 0x0,
                  mask = 0x0,
                  status = 0x0,
                  scroll = 0x0,
                  address = 0x0,
                  pdata = 0x0,
                  oamAddr = 0x0,
                  oamData = 0x0
                  } mem 0 initDisplay
