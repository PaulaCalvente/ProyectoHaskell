import Graphics.Gloss.Interface.Pure.Game

data Orientacion = Vertical | TumbadoIzq | TumbadoDer
  deriving (Eq, Show)

-- Redefino el tipo Estado (Ejercicio 3) para que sea un tipo parametrico, es decir, pasarlo a Functor/Applicative. 
-- Ahora acepta un tipo genérico 'a' para los valores numéricos
data Estado a = Estado
  { posX        :: a
  , posY        :: a
  , velY        :: a
  , orientacion :: Orientacion
  } deriving (Show)
 
-- Instancia Functor: aplica una función a los tres campos numéricos del estado
-- Por ejemplo, fmap (+10) (Estado 0 1 2 Vertical) = Estado 10 11 12 Vertical
instance Functor Estado where
  fmap f (Estado x y v o) = Estado (f x) (f y) (f v) o

-- Instancia Applicative: permite aplicar funciones dentro de un contexto Estado
-- Por ejemplo, (Estado (+1) (*2) (+3) Vertical) <*> (Estado 10 20 30 Vertical),da como resultado Estado 11 40 33 Vertical
instance Applicative Estado where
  pure a = Estado a a a Vertical
  (Estado fx fy fv o1) <*> (Estado x y v o2) =
    Estado (fx x) (fy y) (fv v) (if o1 == Vertical then o2 else o1) 

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

-- pure 0 crea un Estado 0 0 0 Vertical, y luego con (<$>) aplicamos una función que sustituye solo la coordenada X y deja las demás fijas
estadoInicial :: Estado Float
estadoInicial = (\x -> Estado x posYSuelo 0 Vertical) <$> pure 0

estaEnSuelo :: Float -> Bool
estaEnSuelo y = y <= posYSuelo

-- Decide si se muestra el fuego, combina funciones booleanas, se aplican ambas condiciones al mismo Estado sin tener que escribir 'estado' dos veces.
mostrarFuego :: Estado Float -> Bool
mostrarFuego = (||) <$> (not . estaEnSuelo . posY) <*> ((/= Vertical) . orientacion)

dibujarEscena :: Estado Float -> Picture
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

manejarEvento :: Event -> Estado Float -> Estado Float
manejarEvento (EventKey (SpecialKey KeyLeft)  Down _ _) estado = procesarIzquierda estado
manejarEvento (EventKey (SpecialKey KeyRight) Down _ _) estado = procesarDerecha estado
manejarEvento (EventKey (SpecialKey KeyUp)    Down _ _) estado = procesarArriba estado
manejarEvento (EventKey (Char 'a') Down _ _) estado = procesarIzquierda estado
manejarEvento (EventKey (Char 'd') Down _ _) estado = procesarDerecha estado
manejarEvento (EventKey (Char 'w') Down _ _) estado = procesarArriba estado
manejarEvento _ estado = estado


-- En estas funciones (procesarIzquierda,procesarDerecha, procesarArriba), 'fmap' se aplica al Estado completo:
-- modifica simultáneamente todos los valores numéricos (posX, posY, velY)
-- aplicando una función. Aquí solo interesa el desplazamiento en X,
-- pero se usa así para demostrar el concepto de Functor.

procesarIzquierda :: Estado Float -> Estado Float
procesarIzquierda estado
  | orientacion estado == Vertical =
      estado { orientacion = TumbadoIzq }
  | orientacion estado == TumbadoIzq && estaEnSuelo (posY estado) =
      -- fmap aplica la función a todos los campos Float del estado
      fmap (clamp limiteIzq limiteDer . subtract 10) estado
  | orientacion estado == TumbadoDer =
      estado { orientacion = Vertical }
  | otherwise = estado

procesarDerecha :: Estado Float -> Estado Float
procesarDerecha estado
  | orientacion estado == Vertical =
      estado { orientacion = TumbadoDer }
  | orientacion estado == TumbadoDer && estaEnSuelo (posY estado) =
      fmap (\x -> clamp limiteIzq limiteDer (x + 10)) estado
  | orientacion estado == TumbadoIzq =
      estado { orientacion = Vertical }
  | otherwise = estado

procesarArriba :: Estado Float -> Estado Float
procesarArriba estado
  | orientacion estado == Vertical && estaEnSuelo (posY estado) =
      estado { velY = impulsoSalto }
  | orientacion estado /= Vertical =
      estado { orientacion = Vertical }
  | otherwise = estado

-- Aquí se usan fmap, (<$>) y (<*>) para aplicar funciones dentro del contexto del Estado.
-- 'aplicarGravedad' se aplica a la velocidad (velY) usando fmap.
-- Luego se calcula la nueva posición combinando (<$>) y (<*>),
-- mostrando cómo se pueden aplicar funciones a varios campos del Estado a la vez.

actualizar :: Float -> Estado Float -> Estado Float
actualizar dt estado =
  let dt' = realToFrac dt
      aplicarGravedad = (+ (gravedad * dt'))
      nuevaVel = aplicarGravedad <$> fmap velY (pure estado)
      nuevaPos = (+) <$> fmap posY (pure estado) <*> nuevaVel
      (yFinal, vFinal) =
        if posY estado + velY estado * dt' < posYSuelo
        then (posYSuelo, 0)
        else (posY estado + velY estado * dt', velY estado + gravedad * dt')
  in estado { posY = yFinal, velY = vFinal }

clamp :: Float -> Float -> Float -> Float
clamp minimo maximo valor = max minimo (min maximo valor)