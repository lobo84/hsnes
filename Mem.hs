module Mem (
    Memory,
    readMem,
    writeMem,
    initMem
) where

import Data.Map as M


type Memory = M.Map Int Int

isMirror :: Int -> Bool
isMirror addr = 0x2008 <= addr && addr <= 0x3FFF

toMirrorBase :: Int -> Int
toMirrorBase addr = (0x2000 + addr `mod` 8)

readMem :: Int -> Memory -> Int
readMem addr mem
    | isMirror addr = readMem (toMirrorBase addr) mem
    | otherwise     = findWithDefault 0 addr mem
  
writeMem :: Int -> Int -> Memory -> Memory
writeMem addr value mem
    | isMirror addr = writeMem (toMirrorBase addr) value mem
    | otherwise     = M.insert addr value mem

initMem :: [(Int, Int)] -> Memory
initMem mem = Prelude.foldr (\x m -> writeMem (fst x) (snd x) m) empty mem