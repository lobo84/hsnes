module Nes.Internal where

import Cpu
import Ppu
import Mem
import Numeric(showHex)

data Cycles = Cycles {
  cn :: Int, -- base cycles left until next cpu exec
  cd :: Int, -- cpu cycles left of instruction duration
  pn :: Int, -- base cycles left until next ppu exec
  c  :: Int  -- total number of base cycles executed
} deriving Show

runNesInteractive :: (Cpu, Ppu) -> IO ()
runNesInteractive (cpu, ppu) = do go (cpu, ppu, Cycles 0 0 0 0)
                                  return ()
  where go :: (Cpu, Ppu, Cycles) -> IO ()
        go (cpu, ppu, cyc) = 
          do putStrLn (show cpu)
             putStrLn (show cyc)
             wait <- getLine
             go (step cpu ppu cyc)
             return ()


-- ppu:    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
-- cpu:    0        1        2        3        4        5
-- cpu op: [----------------][-------------------------][-----

cpuPrescale = 12
ppuPrescale = 4

step :: Cpu -> Ppu -> Cycles -> (Cpu, Ppu, Cycles)
step cpu ppu cyc@(Cycles cn cd pn c) = (cpu', ppu', Cycles cn' cd' pn' (c+1))
  where
    (cpu', cn', cd') = if cn == 0 && cd == 0 
          then (stepCpu cpu, cpuPrescale-1, 0)
          else (cpu, cn-1, cd)
    (ppu', pn') = if pn == 0
          then (stepPpu ppu, ppuPrescale-1)
          else (ppu, pn-1)





run :: (Cpu, Ppu) -> (Cpu, Ppu, Cycles)
run (cpu, ppu) = go (cpu, ppu, (Cycles 0 0 0 0))
  where go :: (Cpu, Ppu, Cycles) -> (Cpu, Ppu, Cycles)
        go state@(cpu, ppu, cyc@(Cycles cn _ _ _)) 
           | isDeadAtExec cpu cn = state
           | otherwise           = go (step cpu ppu cyc)


