-- Definir los tipos básicos del juego y las funciones básicas de geometría.
-- 1. Tipos a definir:

-- Point: Un punto 2D en el espacio.
type Point = (Float, Float)
-- Vector. Vector siempre se considera que empieza en (0,0)
type Vector = (Int, Int)
--  Angle. Un angulo con decimales
type Angle = Float
-- Distance. Un valor de distancia con decimales.
type Distance = Float
-- Position. Representa la posición de objeto en un mundo 2D.
type Position = (Float, Float)

-- AUXILIAR: TIPO Size. Representa un tamaño en 2D (ancho, alto). Útil para el ejercicio: isInBounds :: Point -> Size -> Bool
type Size = (Float, Float)

--2. Definir las siguientes funciones:

--DistanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio. 
-- Toma dos puntos como entrada y devuelve la distancia lineal que los separa.
distanceBetween :: Position -> Position -> Distance
distanceBetween (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- AngleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo. 
-- Útil para calcular la dirección en la que debe apuntar o moverse un objeto.
angleToTarget :: Position -> Position -> Angle
angleToTarget (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1) * 180 / pi

-- deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.
deg2rad :: Angle -> Angle
deg2rad  a = a * pi / 180

-- rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.
rad2deg :: Angle -> Angle
rad2deg a = a * 180 / pi

-- subVec :: Vector -> Vector -> Vector. Realiza la resta de dos Vectores, devolviendo un nuevo Vector que representa la diferencia entre ellos.
subVec :: Vector -> Vector -> Vector
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.
getVertices :: (Point, Point, Point, Point, Angle) -> [Point]
getVertices (p1, p2, p3, p4, a) =
  map (\(x, y) -> let rad = deg2rad a in (x * cos rad - y * sin rad, x * sin rad + y * cos rad)) [p1, p2, p3, p4]

-- dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como Vectores
dot :: Point -> Point -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.
sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- perp :: Vector -> Vector. Calcula el Vector perpendicular a un punto dado (tratado como Vector).
perp :: Vector -> Vector
perp (x, y) = (-y,x) -- Otra opción es: (y, -x)

-- isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.
isInBounds :: Point -> Size -> Bool
isInBounds (x, y) (width, height) = x >= 0 && x <= width && y >= 0 && y <= height


