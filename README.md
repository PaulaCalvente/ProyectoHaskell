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


## ENTREGA 2

## Analizar, pensar y realizar la declaración de tipos

Se deberían añadir los siguientes tipos:

- Health: Tipo que define la salud del robot, Float.
- IsAlive: Tipo que define si el robot está vivo o muerto, Boolean (No Necesario).
- HaveDetected: Tipo que define si un robot ha detectaado a otro, Boolean.
- NRobots: Número de robots que hay en juego, Int.
- NProjectiles: Número de proyectiles que hay en juego, Int.
- NExplotions: Número de explosiones simultáneas, Int.
- Velocity: Velocidad que toman el robot y proyectiles, (Float, Float).
- Damage: Daño que realiza el proyectil, Float (No Necesario).

Se deberían añadir los siguientes data types:

- Projectile: Reúne los tipos necesarios para crear un proyectil.
- Turret: Reúne los tipos necesarios para crear una torreta.
- Action: Se define por 5 tipos de acciones: Arriba, Abajo, Derecha, Izquierda, Parar.
- Robot: Reúne los tipos necesarios para crear un robot.
- World: Reúne los tipos necesarios para crear un mundo.

## Refactorización de funciones

Para esta función, distanceBetween, el cambio ha sido muy pequeño; se ha añadido un where para definir dx y dy como la diferencia de los componentes de las posiciones, con el fin de sumarle claridad al código.

En la función angleToTarget, se sigue el mismo objetivo que la función anterior; con where se añade claridad al código, indagando en cada operación hecha por separado.

getVertices se ha modificado según la recomendación del profesor, es decir, se sustituye el uso de let-in al uso del where, ya que queda más "declarativa". Además, se cambia la función map por el uso de listas por comprensión.

La última función refactorizada fue isInBounds, que además se ha mejorado la funcionalidad. Por una parte, se ha hecho uso del case-of, estudiando los casos en que cada componente del "Size" sean positivos y/o negativos. Por otra parte, se ha mejorado la funcionalidad de la siguiente manera: Previamente a esta mejora, esta función solamente admitía tratar con componentes positivas, es decir, aunque se introdujera un punto negativo que está dentro de el tamaño dado por parámetro, éste no se consideraba dentro del área. 
NOTA: No sé si este cambio ha sido muy útil, pero prefiero dejar todos los casos posibles cubiertos, incluso los quue podrían no tener sentido (tamaños negativos)

## Implementación de las nuevas funciones

- detectedAgent :: Position -> Position -> Distance -> HaveDetected. 
Hace uso de la función ya definida "distanceBetween" y una operación lógica para calcular si un robot ha detectado a otro.

- isRobotAlive :: Health -> isAlive. 
Si la salud es <0, entonces isAlive será false, e.o.c true.

- countActiveRobots :: [Health] -> NRobots. 
Dada la salud de los robots, calcula cuántos hay activos.

- updateRobotVelocity :: Robot -> Velocity -> Robot. 
Actualiza la velocidad del robot que se pasa por parámetro.

- updateVelocity :: Velocity -> Action -> Velocity. 
Actualiza la velocidad según el movimiento que se toma. Ya que se hace un cambio de sentido, se define una velocidad base (En realidad, me gustaría mejorarlo para que tome la velocidad del robot).

- updatePosition :: Position -> Vector -> Float -> Position. 
Actualiza la posición del robot después de un tiempo según su velocidad.

- mul :: Point -> Point -> Point. 
Multiplica las componentes de dos puntos