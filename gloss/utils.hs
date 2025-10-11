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
dibujarEscena estado = Pictures [escenaFija, Translate x y dibujoAjustado]
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

escenaFija :: Picture
escenaFija = Pictures
  [ cielo
  , sol
  , suelo
  ]
  where
    cielo = Color (makeColor 0 0 0.5 1) $
      Polygon [(-400,-300), (400,-300), (400,300), (-400,300)]
    sol = Translate 200 200 $ Color white $ circleSolid 2
    suelo = Color green $ Translate 0 alturaSuelo $
      Polygon [(-400,0), (400,0), (400,-300), (-400,-300)]

conjuntoMovil :: Picture
conjuntoMovil = Pictures
  [ Color red $ Translate 0 75 $ Polygon [(-50,0), (50,0), (0,70)]
  , Color white $ Translate 0 25 $ rectangleSolid 100 100
  , Color (makeColorI 170 210 90 255) $ Polygon [(-40,60),(40,60),(40,20),(-40,20)]
  , Color (makeColorI 170 210 90 255) $ Polygon [(-40,60),(-80,40),(-40,40)]
  , Color (makeColorI 170 210 90 255) $ Polygon [(40,60),(80,40),(40,40)]
  , Color (makeColorI 100 160 255 255) $ Translate 0 (-30) (rectangleSolid 100 100)
  , Color (makeColorI 0 0 40 255) $
      Line [(-80,40),(-40,60),(40,60),(80,40),(40,40),(50,20),(0,-60),(-50,20),(-40,40),(-80,40)]
  , Color black $ Translate (-15) 40 (circleSolid 5)
  , Color black $ Translate (15) 40 (circleSolid 5)
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
  | orientacion estado == TumbadoIzq && estaEnSuelo (posY estado) =
      estado { posX = clamp limiteIzq limiteDer (posX estado - 10) }
  | orientacion estado == TumbadoDer =
      estado { orientacion = Vertical }
  | otherwise = estado

procesarDerecha :: Estado -> Estado
procesarDerecha estado
  | orientacion estado == Vertical =
      estado { orientacion = TumbadoDer }
  | orientacion estado == TumbadoDer && estaEnSuelo (posY estado) =
      estado { posX = clamp limiteIzq limiteDer (posX estado + 10) }
  | orientacion estado == TumbadoIzq =
      estado { orientacion = Vertical }
  | otherwise = estado

procesarArriba :: Estado -> Estado
procesarArriba estado
  | orientacion estado == Vertical && estaEnSuelo (posY estado) =
      estado { velY = impulsoSalto }
  | orientacion estado /= Vertical =
      estado { orientacion = Vertical }
  | otherwise = estado

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