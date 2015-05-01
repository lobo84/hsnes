module NesMonad(
  Nes(..),
  NesState,
  getCpu,
  putCpu,
  getPpu,
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

getCpu :: NesState Cpu
getCpu = do
  nes <- get
  return (cpu nes)

putCpu :: Cpu -> NesState ()
putCpu cpu = do
  nes <- get
  put (nes { cpu = cpu })

getPpu :: NesState Ppu
getPpu = do
  nes <- get
  return (ppu nes)