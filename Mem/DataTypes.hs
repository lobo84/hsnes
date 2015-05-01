module Mem.DataTypes (
    Memory
) where

import qualified Data.Map as M

type Memory addr value = M.Map addr value
