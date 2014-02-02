module Nes where

import Cpu
import Numeric(showHex)

data Ppu = Ppu

runNesInteractive :: (Cpu, Ppu, Int) -> IO ()
runNesInteractive (cpu, ppu, n) = do
  putStrLn (show cpu)
  --putStrLn ("next op code: " ++ showHex (nextOpCode cpu) "")
  wait <- getLine
  runNesInteractive (step cpu Ppu 0)

step :: Cpu -> Ppu -> Int -> (Cpu, Ppu, Int)
step cpu ppu n = if n == 0 then (cpu', ppu', t) else (cpu, ppu', t - 1)
  where
    cpu' = stepCpu cpu
    t = 0
    ppu' = stepPpu ppu


stepPpu :: Ppu -> Ppu
stepPpu = id


--run :: Int -> Int
--run n = do
--  if n == 0
--    (t  Cpu(regs mem')) <- stepCpu Cpu(regs mem)
--  else 
--    t = n-1
--	ppu <- stepPpu Ppu(mem')
--	wait(fps)
--	step t