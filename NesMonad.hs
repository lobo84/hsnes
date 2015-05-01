module NesMonad(
  Nes,
  NesState,
  startState,
  cpu,
  ppu
  ) where
import Rom as Rom
import Ppu as Ppu
import Cpu as Cpu
import Mem as Mem
import Control.Monad.State

data Nes = Nes {
  cpu :: Cpu,
  ppu :: Ppu
} deriving (Show)

type NesState = StateT Nes IO

startState = Nes (initCpu [] [] False) (initPpu (initPpuMem [])) -- Empty start state

