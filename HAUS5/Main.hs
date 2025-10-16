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
  , explosiones = []
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
  , Pictures (map dibujarExplosion (explosiones m))
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
disparar m = m { chicles = nuovi ++ chicles m }
  where
    ns = ninos m
    -- Para cada niño con índice i, genera un chicle con dirección alterna
    nuovi =
      [ Chicle (x + offset i, y + 10)
               (vx i, 0)
               (makeColorI 180 60 180 230)
               i                  -- ownerIdx = indice del niño
      | (i, Nino (x,y) _ _) <- zip [0..] ns
      ]
    -- alterna derecha/izquierda por índice
    vx i     = if even i then velChicleVel  else -velChicleVel
    offset i = if even i then 20            else -20


-- ================================
-- Actualización
-- ================================

explosionTTL   :: Float; explosionTTL   = 0.6   -- duran 0.6s
explosionGrow  :: Float; explosionGrow  = 200   -- px/s de crecimiento del radio
explosionStart :: Float; explosionStart = 10    -- radio inicial

actualizar :: Float -> Mundo -> Mundo
actualizar dt m
  | modo m == Inicio = m
  | otherwise =
      let ns = ninos m
         -- Recorremos chicles:
          -- - Avanzamos posición
          -- - Si sale por X, lo descartamos
          -- - Si impacta con un niño ≠ owner, eliminamos chicle y creamos explosión
          -- - Si no impacta, lo dejamos seguir
          (keepChicles, newExplosions) = foldr (stepChicle ns) ([], []) (chicles m)

         -- Actualizamos explosiones existentes + añadimos las nuevas:
          -- crecen de radio y decrece su ttl; filtramos las que siguen vivas
          exps' = [ Explosion p (r + explosionGrow*dt) (ttl - dt)
                  | Explosion p r ttl <- explosiones m ++ newExplosions
                  , ttl - dt > 0
                  ]
      in m { chicles = keepChicles, explosiones = exps' }
  where
    dentroX x = x > -ancho/2 && x < ancho/2

        -- Procesa un chicle y acumula (los que siguen, explosiones nuevas)
    stepChicle :: [Nino] -> Chicle -> ([Chicle],[Explosion]) -> ([Chicle],[Explosion])
    stepChicle ns (Chicle (x,y) (vx,vy) col owner) (accC, accE) =
      let x' = x + vx*dt
          y' = y + vy*dt
          pos' = (x',y')
          out  = not (dentroX x')
           -- Impacta si toca a algún niño distinto al owner
          hit  = any (\(j,n) -> j /= owner && circleAABB pos' chicleRadius (ninoBox n))
                     (zip [0..] ns)
      in if out
           then (accC, accE)  -- fuera: eliminado
           else if hit
                  then (accC, Explosion pos' explosionStart explosionTTL : accE) 
                  else (Chicle pos' (vx,vy) col owner : accC, accE)              
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
