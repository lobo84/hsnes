module Mem (
    Memory,
    readMem,
    readMemRange,
    writeMem,
    initMem
) where

import qualified Data.Map as M
import Data.Bits


type Memory addr value = M.Map addr value


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