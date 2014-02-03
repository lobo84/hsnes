module Ppu(
	Ppu,
    stepPpu,
    initPpu
) where

class ToAdress a where
    toAdress :: a -> Int

data Ppu = Ppu

type RegValue = Int

data VramAddrInc = 
      Add1Across
    | Add32Down
    deriving Show


data NameTblBase = 
      X2000
    | X2400
    | X2800
    | X2C00
    deriving Show

instance ToAdress NameTblBase where
    toAdress X2000 = 0x2000
    toAdress X2400 = 0x2400
    toAdress X2800 = 0x2800
    toAdress X2C00 = 0x2C00

data PatTblAddr = 
      X0000
    | X1000
    deriving Show

instance ToAdress PatTblAddr where
    toAdress X0000 = 0x0000
    toAdress X1000 = 0x1000

data SpriteSize = 
      Size8x8
    | Size8x16
    deriving Show

data PpuMasterSlave = 
      ReadBackdrop
    | OutputColor
    deriving Show

data PPUCTRL = PPUCTRL {
  nameTblBase      :: NameTblBase,    -- Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
  vramAddrInc      :: VramAddrInc,    -- RAM address increment per CPU read/write of PPUDATA, (0: add 1, going across; 1: add 32, going down)
  spritePatTblAddr :: PatTblAddr,     -- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
  bgPatTblAddr     :: PatTblAddr,     -- Background pattern table address (0: $0000; 1: $1000)
  spriteSize       :: SpriteSize,     -- Sprite size (0: 8x8; 1: 8x16)
  ppuMasterSlave   :: PpuMasterSlave, -- PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins)
  generateNMI      :: Bool            -- Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
} deriving Show

data PPUMASK = PPUMASK {
    grayscale           :: Bool, -- Grayscale (0: normal color; 1: produce a monochrome display)
    showLeftmostBg      :: Bool, -- 1: Show background in leftmost 8 pixels of screen; 0: Hide
    showLeftmostSprites :: Bool, -- 1: Show sprites in leftmost 8 pixels of screen; 0: Hide
    showBg              :: Bool, -- 1: Show background
    showSprites         :: Bool, -- 1: Show sprites
    intensifyReds       :: Bool, -- Intensify reds (and darken other colors)
    intensifyGreens     :: Bool, -- Intensify greens (and darken other colors)
    intensifyBlues      :: Bool  -- Intensify blues (and darken other colors)
}

data PPUSTATUS = PPUSTATUS {
    spriteOverflow :: Bool, -- Sprite overflow. The intent was for this flag to be set
                            -- whenever more than eight sprites appear on a scanline, but a
                            -- hardware bug causes the actual behavior to be more complicated
                            -- and generate false positives as well as false negatives; see
                            -- PPU sprite evaluation. This flag is set during sprite
                            -- evaluation and cleared at dot 1 (the second dot) of the
                            -- pre-render line.
    spriteZeroHit :: Bool,  -- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
                            -- a nonzero background pixel; cleared at dot 1 of the pre-render
                            -- line.  Used for raster timing.
    vblankStarted :: Bool   -- Vertical blank has started (0: not in VBLANK; 1: in VBLANK).
                            -- Set at dot 1 of line 241 (the line *after* the post-render
                            -- line); cleared after reading $2002 and at dot 1 of the
                            -- pre-render line.

}

{-

-}

stepPpu :: Ppu -> Ppu
stepPpu = id

initPpu :: Ppu
initPpu = Ppu