import Graphics.Gloss.Interface.Pure.Game

data Orientacion = Vertical | TumbadoIzq | TumbadoDer
  deriving (Eq, Show)

data Estado = Estado
  { posX        :: Float
  , posY        :: Float
  , velY        :: Float
  , orientacion :: Orientacion
  } deriving (Show)

-- Parámetros físicos
gravedad :: Float
gravedad = -200
impulsoSalto :: Float
impulsoSalto = 150

-- Geometría del dibujo
radioInferior :: Float
radioInferior = 60  -- punto más bajo del monigote/cohete en coordenadas locales

alturaSuelo :: Float
alturaSuelo = -150  -- coordenada Y del borde superior del suelo

-- Posición Y en la que el centro del cohete toca el suelo
posYSuelo :: Float
posYSuelo = alturaSuelo + radioInferior

-- Límites de la ventana
anchoVentana, altoVentana :: Float
anchoVentana = 600
altoVentana = 600

-- Ancho estimado para colisión (±80)
anchoConjunto :: Float
anchoConjunto = 160

limiteIzq, limiteDer :: Float
limiteIzq = -anchoVentana / 2 + anchoConjunto / 2
limiteDer =  anchoVentana / 2 - anchoConjunto / 2

main :: IO ()
main = play
  (InWindow "Figura pixel art" (round anchoVentana, round altoVentana) (100, 100))
  white
  60
  estadoInicial
  dibujarEscena
  manejarEvento
  actualizar

estadoInicial :: Estado
estadoInicial = Estado 0 posYSuelo 0 Vertical

estaEnSuelo :: Float -> Bool
estaEnSuelo y = y <= posYSuelo

-- Decide si se muestra el fuego
mostrarFuego :: Estado -> Bool
mostrarFuego estado =
  not (estaEnSuelo (posY estado)) ||  -- en el aire (saltando)
  orientacion estado /= Vertical       -- o tumbado en el suelo

dibujarEscena :: Estado -> Picture
dibujarEscena estado = Pictures
  [ escenaFija
  , Translate x y dibujoAjustado
  , hudInfo estado
  ]
  where
    x = posX estado
    y = posY estado
    -- Rotación alrededor del punto inferior (0, -radioInferior) para mantener contacto con el suelo
    dibujoAjustado = case orientacion estado of
      Vertical    -> dibujoConFuego
      TumbadoIzq  -> Translate 0 (-radioInferior) $ Rotate (-90) $ Translate 0 radioInferior dibujoConFuego
      TumbadoDer  -> Translate 0 (-radioInferior) $ Rotate (90)  $ Translate 0 radioInferior dibujoConFuego

    dibujoConFuego = Pictures $
      [ conjuntoMovil
      ] ++ if mostrarFuego estado then [fuegoCohete] else []

-- HUD: dos líneas compactas en la esquina superior izquierda
hudInfo :: Estado -> Picture
hudInfo estado = Pictures
  [ Translate (-anchoVentana/2 + 12) (altoVentana/2 - 25) $
      Scale 0.12 0.12 $
      Color white $
      Text ("Estado: " ++ estadoSuelo ++ " | Pos X: " ++ posXStr)
  , Translate (-anchoVentana/2 + 12) (altoVentana/2 - 45) $
      Scale 0.12 0.12 $
      Color white $
      Text "Controles: A/D ←/→ = Girar/Mover | W/↑ = Saltar"
  ]
  where
    estadoSuelo = if estaEnSuelo (posY estado) then "Suelo" else "Aire"
    posXStr = show (round (posX estado))

escenaFija :: Picture
escenaFija = Pictures
  [ cielo
  , estrellas
  , suelo
  ]
  where
    cielo = Color (makeColor 0 0 0.5 1) $
      Polygon [(-400,-300), (400,-300), (400,300), (-400,300)]

    estrellas = Translate 0 (-150) $ Pictures
      [ Translate x y $ Color white $ circleSolid r
      | (x, y, r) <-
          [ (-380, 260, 2.5), (-340, 180, 1.5), (-300, 120, 1.2)
          , (-260, 240, 1.8), (-220, 60, 2.0), (-180, 280, 1.3)
          , (-140, 150, 2.2), (-100, 220, 1.0), (-60, 260, 1.7)
          , (-20, 180, 1.5), (20, 300, 2.5), (60, 240, 1.3)
          , (100, 180, 1.8), (140, 100, 2.1), (180, 260, 1.2)
          , (220, 200, 2.0), (260, 300, 1.7), (300, 140, 1.4)
          , (340, 220, 1.6), (380, 180, 2.3)
          ]
      ]

    suelo = Color (makeColorI 20 100 20 255) $
      Translate 0 alturaSuelo $
        Polygon [(-400,0), (400,0), (400,-300), (-400,-300)]


conjuntoMovil :: Picture
conjuntoMovil = Pictures
  [
    Color red $ Translate 0 75 $ Polygon [(-50,35), (50,35), (0,100)],
    Color (makeColorI 220 220 220 255) $ Translate 0 25 $ rectangleSolid 100 170,
    Color red $ Polygon [(-50,-60), (-90,-70), (-50,-20)],  -- izquierda
    Color red $ Polygon [(50,-60), (90,-70), (50,-20)],
    Color (makeColorI 170 170 170 255) $ Translate 0 70 (circleSolid 25),

    Translate 0 20 $ Scale 0.5 0.5 $ Pictures
      [ Color (makeColorI 170 210 90 255) $
          Polygon [(-40,10),(40,10),(40,-30),(-40,-30)],
        Color (makeColorI 170 210 90 255) $
          Polygon [(-40,-10),(-80,-30),(-40,10)],
        Color (makeColorI 170 210 90 255) $
          Polygon [(40,-10),(80,-30),(40,10)],
        Color (makeColorI 100 160 255 255) $
          Translate 0 (-80) (rectangleSolid 80 100)
      ],
    Color black $ Translate (-12) 15 (circleSolid 4),
    Color black $ Translate 12 15 (circleSolid 4)
  ]

fuegoCohete :: Picture
fuegoCohete = Color (makeColorI 255 100 0 200) $
  Polygon [ (-20, -60), (20, -60), (0, -90) ]

manejarEvento :: Event -> Estado -> Estado
manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) estado = procesarIzquierda estado
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) estado = procesarDerecha estado
manejarEvento (EventKey (SpecialKey KeyUp)    Down _ _) estado = procesarArriba estado
manejarEvento (EventKey (Char 'a') Down _ _) estado = procesarIzquierda estado
manejarEvento (EventKey (Char 'd') Down _ _) estado = procesarDerecha estado
manejarEvento (EventKey (Char 'w') Down _ _) estado = procesarArriba estado
manejarEvento _ estado = estado

procesarIzquierda :: Estado -> Estado
procesarIzquierda estado
  | orientacion estado == Vertical =
      estado { orientacion = TumbadoIzq }
  | orientacion estado == TumbadoIzq =
      estado { posX = clamp limiteIzq limiteDer (posX estado - 10) }
  | orientacion estado == TumbadoDer =
      estado { orientacion = Vertical }
  | otherwise = estado

procesarDerecha :: Estado -> Estado
procesarDerecha estado
  | orientacion estado == Vertical =
      estado { orientacion = TumbadoDer }
  | orientacion estado == TumbadoDer =
      estado { posX = clamp limiteIzq limiteDer (posX estado + 10) }
  | orientacion estado == TumbadoIzq =
      estado { orientacion = Vertical }
  | otherwise = estado


procesarArriba :: Estado -> Estado
procesarArriba estado =
  estado { velY = impulsoSalto }

actualizar :: Float -> Estado -> Estado
actualizar dt estado = estado'
  where
    dt' = realToFrac dt
    nuevaVelY = velY estado + gravedad * dt'
    nuevaPosY = posY estado + nuevaVelY * dt'

    (posYCorregida, velYCorregida) =
      if nuevaPosY < posYSuelo
        then (posYSuelo, 0)
        else (nuevaPosY, nuevaVelY)

    estado' = estado
      { posY = posYCorregida
      , velY = velYCorregida
      }

clamp :: Float -> Float -> Float -> Float
clamp minimo maximo valor = max minimo (min maximo valor)