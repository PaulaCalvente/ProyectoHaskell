module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Utils

-- ================================
-- Estado inicial
-- ================================

estadoInicial :: Picture -> Picture -> Mundo
estadoInicial imagenInicio imagenFondo = Mundo
  { ninos  =
      [ Nino (-150, -100) orange False
      , Nino (150, -100) blue False
      , Nino (-150, 50) red False
      , Nino (150, 50) green False
      ]
  , chicles = []
  , profe   = (0, 160)
  , modo    = Inicio
  , imagenInicio = imagenInicio  -- pantalla de inicio BMP
  , fondoJuego   = imagenFondo   -- fondo del juego BMP
  }

-- ================================
-- Dibujar
-- ================================

dibujar :: Mundo -> Picture
dibujar m =
  case modo m of
    Inicio  -> dibujarPantallaInicio m
    Jugando -> dibujarJuego m

dibujarJuego :: Mundo -> Picture
dibujarJuego m = Pictures
  [ fondoJuego m
  , dibujarProfe (profe m)
  , Pictures (map dibujarNino (ninos m))
  , Pictures (map dibujarChicle (chicles m))
  ]


dibujarPantallaInicio :: Mundo -> Picture
dibujarPantallaInicio m = Pictures
  [ imagenInicio m
  , dibujarBoton
  ]

-- ================================
-- Botón
-- ================================

botonRect :: ((Float, Float), (Float, Float))
botonRect = ((-115, -185), (85, -95))  -- ajustado al Translate del botón

dibujarBoton :: Picture
dibujarBoton = Pictures
  [ Translate (-85) (-140) $ Scale 0.2 0.2 $ Color black $ Text "Iniciar Juego"
  ]

dentroBoton :: (Float, Float) -> Bool
dentroBoton (mx, my) =
  let ((x1, y1), (x2, y2)) = botonRect
  in mx >= x1 && mx <= x2 && my >= y1 && my <= y2

-- ================================
-- Eventos
-- ================================

manejarEvento :: Event -> Mundo -> Mundo
-- Clic en el botón para iniciar
manejarEvento (EventKey (MouseButton LeftButton) Down _ (mx, my)) m
  | modo m == Inicio, dentroBoton (mx, my) = m { modo = Jugando }
  | otherwise = m

-- Controles normales
manejarEvento (EventKey (SpecialKey KeyUp) Down _ _) m
  | modo m == Jugando = moverNino 0 10 m
manejarEvento (EventKey (SpecialKey KeyDown) Down _ _) m
  | modo m == Jugando = moverNino 0 (-10) m
manejarEvento (EventKey (SpecialKey KeyLeft) Down _ _) m
  | modo m == Jugando = moverNino (-10) 0 m
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) m
  | modo m == Jugando = moverNino 10 0 m
manejarEvento (EventKey (SpecialKey KeySpace) Down _ _) m
  | modo m == Jugando = disparar m
manejarEvento _ m = m

-- ================================
-- Movimiento y disparo
-- ================================

moverNino :: Float -> Float -> Mundo -> Mundo
moverNino dx dy m = m { ninos = map mover (ninos m) }
  where mover (Nino (x, y) c d) = Nino (x + dx, y + dy) c d

disparar :: Mundo -> Mundo
disparar m = m { chicles = nuevosChicles ++ chicles m }
  where
    nuevosChicles =
      [ Chicle (x + offset, y + 10) (vx, 0) (makeColorI 180 60 180 230)
      | (Nino (x,y) _ _, vx, offset) <- zip3 (ninos m) direcciones offsetX
      ]
    direcciones = cycle [ velChicleVel, -velChicleVel ]
    offsetX = cycle [ 20, -20 ]

-- ================================
-- Actualización
-- ================================

actualizar :: Float -> Mundo -> Mundo
actualizar dt m
  | modo m == Inicio = m
  | otherwise = m
      { chicles = [ actualizarChicle c | c <- chicles m, dentro (posChicle c) ] }
  where
    actualizarChicle (Chicle (x,y) (vx,vy) col) =
      let nuevoX = x + vx * dt
          nuevoY = y + vy * dt
      in Chicle (nuevoX, nuevoY) (vx, vy) col

    dentro (x,_) = x < ancho/2 && x > -ancho/2

-- ================================
-- Main
-- ================================

main :: IO ()
main = do
  inicio <- loadBMP "inicio.bmp"   -- pantalla de inicio
  clase  <- loadBMP "clase.bmp"    -- fondo del juego
  play
    (InWindow "Niños y Chicles" (round ancho, round alto) (100, 100))
    white
    60
    (estadoInicial inicio clase)
    dibujar
    manejarEvento
    actualizar
