# ProyectoHaskell

## ENTREGA 1

### Tipos 
Point: Un punto 2D en el espacio.
Vector. Vector siempre se considera que empieza en (0,0)
Angle. Un angulo con decimales
Distance. Un valor de distancia con decimales.
Position. Representa la posición de objeto en un mundo 2D.

AUXILIAR: TIPO Size. Representa un tamaño en 2D (ancho, alto). Útil para el ejercicio: isInBounds :: Point -> Size -> Bool

### Funciones

DistanceBetween :: Position -> Position -> Distance. Calcula la distancia euclidiana entre dos posiciones en el espacio.

AngleToTarget :: Position -> Position -> Angle. Determina el ángulo desde una posición origen hacia una posición objetivo.  Útil para calcular la dirección en la que debe apuntar o moverse un objeto.

deg2rad :: Angle -> Angle. Convierte un ángulo expresado en grados a su equivalente en radianes.

rad2deg :: Angle -> Angle. Convierte un ángulo expresado en radianes a su equivalente en grados.

subVec :: Vector -> Vector -> Vector. Realiza la resta de dos Vectores, devolviendo un nuevo Vector que representa la diferencia entre ellos.

getVertices :: (Point, Point, Point, Point, Angle) -> [Point]. Genera una lista de vértices (puntos) a partir de cuatro puntos base y un ángulo de rotación.

dot :: Point -> Point -> Float. Calcula el producto escalar (dot product) entre dos puntos tratados como Vectores

sub :: Point -> Point -> Point. Resta un punto de otro, devolviendo un nuevo punto que representa la diferencia entre las coordenadas.

perp :: Vector -> Vector. Calcula el Vector perpendicular a un punto dado (tratado como Vector).

isInBounds :: Point -> Size -> Bool. Verifica si un punto se encuentra dentro de los límites definidos por un tamaño dado.
