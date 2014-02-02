module Mem (
    Memory,
    readMem,
    writeMem,
    initMem
) where

import qualified Data.Map as M


type Memory = M.Map Int Int

readMem :: Int -> Memory -> Int
readMem addr mem | M.member addr mem == True = (M.!) mem addr
                 | M.member addr mem == False = 0
  
writeMem :: Int -> Int -> Memory -> Memory
writeMem address value mem = (M.insert) address value mem

initMem :: [(Int, Int)] -> Memory
initMem = M.fromList