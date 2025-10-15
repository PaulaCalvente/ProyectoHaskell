module Main where

import Graphics.Gloss.Interface.Pure.Game
import Utils

-- ================================
-- Estado inicial
-- ================================

estadoInicial :: Mundo
estadoInicial = Mundo
  { ninos =
      [ Nino (-150, -100) orange False
      , Nino (150, -100) blue False
      , Nino (-150, 50) red False
      , Nino (150, 50) green False
      ]
  , chicles = []
  , profe = (0, 160)
  }

-- ================================
-- Dibujo de la escena
-- ================================

dibujar :: Mundo -> Picture
dibujar m = Pictures
  [ dibujarFondoAula
  , dibujarSuelo
  , dibujarPizarra
  , dibujarPuerta
  , dibujarBancos
  , dibujarProfe (profe m)
  , Pictures (map dibujarNino (ninos m))
  , Pictures (map dibujarChicle (chicles m))
  ]

-- ================================
-- Eventos del teclado
-- ================================

manejarEvento :: Event -> Mundo -> Mundo
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m =
  moverNino (-1) m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m =
  moverNino 1 m
manejarEvento (EventKey (SpecialKey KeySpace) Down _ _) m =
  disparar m
manejarEvento _ m = m

moverNino :: Float -> Mundo -> Mundo
moverNino dir m = m { ninos = map mover (ninos m) }
  where mover (Nino (x,y) c d) = Nino (x + dir * 10, y) c d

disparar :: Mundo -> Mundo
disparar m = m { chicles = nuevosChicles ++ chicles m }
  where
    nuevosChicles =
      [ Chicle (x + offset, y + 10) (vx, 0) (makeColorI 180 60 180 230)
      | (Nino (x,y) _ _, vx, offset) <- zip3 (ninos m) direcciones offsetX
      ]
    -- cada niño tiene una dirección diferente (derecha o izquierda)
    direcciones = cycle [ velChicleVel, -velChicleVel ]
    -- pequeño desplazamiento para disparar frente a la boca
    offsetX = cycle [ 20, -20 ]

-- ================================
-- Actualización del mundo
-- ================================

actualizar :: Float -> Mundo -> Mundo
actualizar dt m = m
  { chicles = [ actualizarChicle c | c <- chicles m, dentro (posChicle c) ] }
  where
    actualizarChicle :: Chicle -> Chicle
    actualizarChicle (Chicle (x,y) (vx,vy) col) =
      let nuevoX = x + vx * dt
          nuevoY = y + vy * dt
      in Chicle (nuevoX, nuevoY) (vx, vy) col

    dentro (x,_) = x < ancho/2 && x > -ancho/2

-- ================================
-- Función principal
-- ================================

main :: IO ()
main = play
  (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
  white
  60
  estadoInicial
  dibujar
  manejarEvento
  actualizar