module Cpu.DataTypes(
  Cpu(..),
  Registers(..),
  RegisterType(..),
  RegValue,
  Cyc,
  State(..),
  Controller(..),
  Mode(..)
) where

import Mem.DataTypes
import Text.Printf

data Cpu = Cpu { memory :: Memory Int Int
               , registers :: Registers
               , cyc :: Int
               , state :: State
               , debug :: Bool
               } deriving Show

data Registers = Registers {
  pc :: RegValue,
  status :: RegValue,
  acc :: RegValue,
  x :: RegValue,
  y :: RegValue,
  sp :: RegValue
}

instance Show Registers where
  show (Registers pc status acc x y sp) =
    "{" ++
    "pc=" ++ h pc ++
    " status=" ++ h status ++
    " acc=" ++ h acc ++
    " x=" ++ h x ++
    " y=" ++ h y ++
    " sp=" ++ h sp ++
    "}"
    where h val = showB val

showB v = printf "%02X" v

data RegisterType = Pc | Status | Acc | X | Y | Sp deriving (Eq, Show)

type RegValue = Int
type Cyc = Int

data State = State {
  mode :: Mode,
  controllers :: (Controller, Controller)
} deriving (Show)

data Controller = Controller {
  up :: Bool,
  down :: Bool,
  left :: Bool,
  right :: Bool,
  a :: Bool,
  b :: Bool,
  select :: Bool,
  start :: Bool,
  nextRead :: Int
} deriving (Show)

data Mode = Running
          | Halted deriving (Eq, Show)
