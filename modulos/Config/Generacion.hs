module Config.Generacion where

import Test.QuickCheck (Gen, generate, choose)
import Utils (distanceBetween)
import Config.Dibujar (estaDentroDeEscritorio)
import Data.Mundo (ancho, alto)

------------------------------------------------------------
-- GENERACIÓN ALEATORIA DE POSICIONES
------------------------------------------------------------
genPosicion :: Gen (Float, Float)
genPosicion = do
  x <- choose (-ancho/2, ancho/2)
  y <- choose (-alto/2,  alto/2)
  pure (x, y)

distMin :: Float
distMin = 80

genPosicionUnica :: [(Float, Float)] -> Gen (Float, Float)
genPosicionUnica existentes = do
  p <- genPosicion
  if all (\q -> distanceBetween p q > distMin) existentes
     && not (estaDentroDeEscritorio p)
     then return p
     else genPosicionUnica existentes

generarPosiciones :: Int -> Gen [(Float, Float)]
generarPosiciones n = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
      p <- genPosicionUnica acc
      go (k - 1) (p : acc)
