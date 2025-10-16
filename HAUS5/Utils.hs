module Utils where
import Graphics.Gloss

-- ================================
-- Tipos de datos
-- ================================

data Nino = Nino
  { posNino :: (Float, Float)
  , colorNino :: Color
  , disparando :: Bool
  } deriving (Show)

data Chicle = Chicle
  { posChicle :: (Float, Float)
  , velChicle :: (Float, Float)
  , colorChicle :: Color
  , ownerIdx :: Int --ownerIdx (índice del niño que lo lanzó) para
                    -- ignorar la auto-colisión en el primer frame
  } deriving (Show)

-- Radio que usamos para colisión del chicle
chicleRadius :: Float
chicleRadius = 10

data Explosion = Explosion
  { posExp  :: (Float, Float) --centro de la explosión
  , radExp  :: Float      -- radio actual
  , ttlExp  :: Float      -- tiempo restante
  } deriving (Show)

data Mundo = Mundo
  { ninos        :: [Nino]
  , chicles      :: [Chicle]
  , profe        :: (Float, Float)
  , modo         :: ModoJuego
  , imagenInicio :: Picture   
  , fondoJuego   :: Picture 
  , explosiones :: [Explosion]
  }

-- Caja aproximada del niño (para colisión), centrada en su pos
ninoBox :: Nino -> ((Float,Float),(Float,Float))
ninoBox (Nino (x,y) _ _) = ((x-20,y-25),(x+20,y+25))

data ModoJuego = Inicio | Jugando
  deriving (Eq, Show)

-- Collision entre chicle y rectangulo (niño)
circleAABB :: (Float,Float) -> Float -> ((Float,Float),(Float,Float)) -> Bool
circleAABB (cx,cy) r ((minx,miny),(maxx,maxy)) =
  let clx = max minx (min cx maxx)
      cly = max miny (min cy maxy)
      dx  = cx - clx
      dy  = cy - cly
  in dx*dx + dy*dy <= r*r

-- ================================
-- Constantes generales
-- ================================

ancho, alto :: Float
ancho = 600
alto  = 600

velNino, velChicleVel :: Float
velNino = 150
velChicleVel = 300

-- ================================
-- Dibujo de los elementos
-- ================================

dibujarProfe :: (Float, Float) -> Picture
dibujarProfe (x,y) = Translate x y $ Pictures
  [ -- cabeza
    Color (makeColorI 255 220 180 255) $ Translate 0 40 $ rectangleSolid 40 40
    -- cabello
  , Translate 0 55 $ Color (makeColorI 80 50 10 255) $ rectangleSolid 42 10
    -- cuerpo
  , Color (makeColorI 40 70 160 255) $ rectangleSolid 50 60
    -- pantalones
  , Translate 0 (-50) $ Color (makeColorI 30 40 80 255) $ rectangleSolid 40 25
    -- ojos
  , Translate (-10) 45 $ Color black $ circleSolid 3
  , Translate (10) 45 $ Color black $ circleSolid 3
    -- boca
  , Translate 0 32 $ Color red $ rectangleSolid 10 3
  ]

dibujarNino :: Nino -> Picture
dibujarNino (Nino (x,y) c _) = Translate x y $ Pictures
  [ -- cuerpo
    Color c $ rectangleSolid 40 50
    -- cabeza
  , Translate 0 35 $ Color (makeColorI 255 220 180 255) $ rectangleSolid 30 30
    -- cabello
  , Translate 0 50 $ Color (makeColorI 90 60 20 255) $ rectangleSolid 32 8
    -- ojos
  , Translate (-8) 40 $ Color black $ circleSolid 2.5
  , Translate (8) 40 $ Color black $ circleSolid 2.5
    -- boca
  , Translate 0 28 $ Color red $ rectangleSolid 8 2
    -- piernas
  , Translate 0 (-45) $ Color black $ rectangleSolid 30 10
    -- chicle rosa oscuro
  , Translate 18 28 $ Color (makeColorI 180 60 180 230) $ circleSolid 6
  ]

dibujarChicle :: Chicle -> Picture
dibujarChicle (Chicle (x,y) _ c _owner) =
  let r = 8 + 2 * sin (x / 30)  
  in Translate x y $ Color c $ circleSolid r

-- Dibuja una explosión con dos capas y alpha según ttlExp (vida restante).
dibujarExplosion :: Explosion -> Picture
dibujarExplosion (Explosion (x,y) r ttl) =
  let a = max 0 (min 1 (ttl / 0.6))   -- alfa basado en ttl( ttl = 0.6 → alpha ≈ 1; ttl → 0 → alpha → 0);
  in Translate x y $
       Pictures
         [ Color (withAlpha a (makeColorI 255 120 200 255)) $ thickCircle (r*0.6) (r*0.25)
         , Color (withAlpha (a*0.8) (makeColorI 255 200 255 255)) $ circleSolid (r*0.3)
         ]


-- ================================
-- Fondo y suelo del aula
-- ================================

dibujarFondoAula :: Picture
dibujarFondoAula = Color (makeColorI 255 240 200 255) $
  rectangleSolid ancho alto

dibujarSuelo :: Picture
dibujarSuelo =
  Color (makeColorI 220 180 120 255) $
    Translate 0 (-100) $ rectangleSolid ancho 400

-- ================================
-- Pizarra y puerta
-- ================================

dibujarPizarra :: Picture
dibujarPizarra =
  Translate (-100) 230 $
    Color (makeColorI 30 80 130 255) $ rectangleSolid 220 70

dibujarPuerta :: Picture
dibujarPuerta =
  Translate 250 160 $ Pictures
    [ Color (makeColorI 100 60 20 255) $ rectangleSolid 60 160
    , Translate 20 0 $ Color (makeColorI 230 200 100 255) $ circleSolid 4  -- pomo
    ]

-- ================================
-- Bancos del aula
-- ================================

dibujarBancos :: Picture
dibujarBancos =
  Pictures
    [ banco (-150) (-100)
    , banco (150) (-100)
    , banco (-150) (50)
    , banco (150) (50)
    ]
  where 
    banco x y = Translate x y $
      Pictures
        [ Color (makeColorI 190 120 60 255) $ rectangleSolid 100 40   -- tablero del banco
        , Translate 0 (-25) $ Color (makeColorI 130 80 40 255) $ rectangleSolid 90 10  -- patas
        ]