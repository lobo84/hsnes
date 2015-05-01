module Mem (
    Memory,
    readMem,
    readMemRange,
    writeMem,
    initMem,
    readPpuMem,
    readPpuMemRange,
    writePpuMem,
    initPpuMem
) where

import qualified Data.Map as M
import Data.Bits


type Memory addr value = M.Map addr value


-- CPU RAM

isMirror addr = 0x2008 <= addr && addr <= 0x3FFF

toMirrorBase addr = (0x2000 + addr `mod` 8)

readMem addr mem
    | isMirror addr = readMem (toMirrorBase addr) mem
    | otherwise     = M.findWithDefault 0 addr mem
  
readMemRange baseAddr end mem
  | end < baseAddr = error "invalid range"
  | otherwise = map (\a -> readMem a mem) [baseAddr..end]
  

writeMem addr value mem
    | isMirror addr = writeMem (toMirrorBase addr) value mem
    | otherwise     = M.insert addr value mem

initMem mem = Prelude.foldr (\x m -> writeMem (fst x) (snd x) m) M.empty mem


-- PPU VRAM

isNametableMirror addr = 0x3000 <= addr && addr <= 0x3EFF -- mirrors 0x2000 - 0x2EFF
isPaletteMirror addr = 0x3F20 <= addr && addr <= 0x3FFF -- mirrors 

toNametableMirrorBase addr = (addr - 0x1000)
toPaletteMirrorBase addr = (0x3F00 + addr `mod` 32)

readPpuMem addr mem
    | isNametableMirror addr = readPpuMem (toNametableMirrorBase addr) mem
    | isPaletteMirror addr = readPpuMem (toPaletteMirrorBase addr) mem
    | otherwise     = M.findWithDefault 0 addr mem
  
readPpuMemRange baseAddr end mem
  | end < baseAddr = error "invalid range"
  | otherwise = map (\a -> readPpuMem a mem) [baseAddr..end]
  

writePpuMem addr value mem
    | isNametableMirror addr = writePpuMem (toNametableMirrorBase addr) value mem
    | isPaletteMirror addr = writePpuMem (toPaletteMirrorBase addr) value mem
    | otherwise     = M.insert addr value mem

initPpuMem mem = Prelude.foldr (\x m -> writePpuMem (fst x) (snd x) m) M.empty mem