module Nes (runNesInteractive, step) where
import Nes.Internal






--run :: Int -> Int
--run n = do
--  if n == 0
--    (t  Cpu(regs mem')) <- stepCpu Cpu(regs mem)
--  else 
--    t = n-1
--  ppu <- stepPpu Ppu(mem')
--  wait(fps)
--  step t