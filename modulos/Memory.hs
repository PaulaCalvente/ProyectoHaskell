module Memory
  ( MemoryValue(..)
  , Memory
  ) where

import Utils            -- Punto definido en Utils.hs
import qualified Data.Map as M


-- Valores que un agente puede recordar
data MemoryValue
  = MemInt Int
  | MemFloat Float
  | MemString String
  | MemPoint Point
  | MemBool Bool
  deriving (Show, Eq)

-- Diccionario de memoria
type Memory = M.Map . MemoryValue