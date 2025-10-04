
module Memory
  ( MemoryValue(..)
  , Memory
  , emptyMemory
  , setMemory
  , getMemory
  ) where

import Game.Utils (Point)
import qualified Data.Map as M
import GHC.Generics (Generic)

-- Valores que un agente puede recordar
data MemoryValue
  = MemInt Int
  | MemFloat Float
  | MemString String
  | MemPoint Point
  | MemBool Bool
  deriving (Show, Eq, Generic)

-- Diccionario de memoria
type Memory = M.Map String MemoryValue