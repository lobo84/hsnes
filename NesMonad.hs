module NesMonad(
  Nes(..),
  NesState,
) where

import Rom as Rom
import Ppu as Ppu
import Cpu.DataTypes as Cpu
import Mem as Mem
import Control.Monad.State

data Nes = Nes {
  cpu :: Cpu,
  ppu :: Ppu
} deriving (Show)

type NesState = StateT Nes IO

