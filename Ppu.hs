module Ppu(
	Ppu,
    stepPpu,
    initPpu
) where

data Ppu = Ppu

stepPpu :: Ppu -> Ppu
stepPpu = id

initPpu :: Ppu
initPpu = Ppu