module Nes where

import Cpu
import Ppu
import Numeric(showHex)


runNesInteractive :: (Cpu, Ppu, Int) -> IO ()
runNesInteractive (cpu, ppu, n) = do
  putStrLn (show cpu)
  --putStrLn ("next op code: " ++ showHex (nextOpCode cpu) "")
  wait <- getLine
  runNesInteractive (step cpu ppu 0)

step :: Cpu -> Ppu -> Int -> (Cpu, Ppu, Int)
step cpu ppu n = 
    if n == 0 
    then (cpu', ppu', t) 
    else (cpu, ppu', t - 1)
  where
    cpu' = stepCpu cpu
    ppu' = stepPpu ppu
    t = 0





--run :: Int -> Int
--run n = do
--  if n == 0
--    (t  Cpu(regs mem')) <- stepCpu Cpu(regs mem)
--  else 
--    t = n-1
--  ppu <- stepPpu Ppu(mem')
--  wait(fps)
--  step t