module Memory
  ( MemoryValue(..)
  , Memory
  ) where

import Utils -- Punto definido en Utils.hs
import qualified Data.Map as M

-- Valores que un agente puede recordar
data MemoryValue a
  = MemInt a
  | MemFloat a
  | MemPoint a
  | MemString String
  | MemBool Bool
  deriving (Show, Eq)

instance Functor MemoryValue where
  fmap f (MemInt x)    = MemInt (f x)
  fmap f (MemFloat x)  = MemFloat (f x)
  fmap f (MemPoint x)  = MemPoint (f x)
  fmap _ (MemString s) = MemString s
  fmap _ (MemBool b)   = MemBool b

instance Applicative MemoryValue where
  pure x = MemFloat x
  (MemInt f)   <*> (MemInt x)   = MemInt (f x)
  (MemFloat f) <*> (MemFloat x) = MemFloat (f x)
  (MemPoint f) <*> (MemPoint x) = MemPoint (f x)
  _ <*> v = v -- Si no coincide ninguno de los patrones anteriores devuelve simplemente v

-- Diccionario de memoria
type Memory = M.Map String (MemoryValue Float)