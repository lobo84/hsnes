module Ppu where

import Data.Bits
import qualified Data.Map as Map

class ToAdress a where
    toAdress :: a -> Int

data Ppu = Ppu

type RegValue = Int

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

bitToValue :: Int -> Int -> (a, a) -> a
bitToValue reg bit (f, t) = case (testBit reg bit) of
                              False -> f
                              True  -> t

parsePPUCTRL :: Int -> PPUCTRL
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

parsePPUMASK :: Int -> PPUMASK
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

parsePPUSTATUS :: Int -> PPUSTATUS
parsePPUSTATUS d = PPUSTATUS vblankStarted spriteZeroHit spriteOverflow
  where
    vblankStarted  = (testBit d 7)
    spriteZeroHit  = (testBit d 6)
    spriteOverflow = (testBit d 5)

stepPpu :: Ppu -> Ppu
stepPpu = id

initPpu :: Ppu
initPpu = Ppu