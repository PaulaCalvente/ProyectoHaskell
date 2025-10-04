--EJERCICIO DE HASKELL

-- DECLARACION DE TIPOS INICIALES
type Point = (Float, Float) 
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = Point
type Size = (Float, Float)

-- 1. Analiza el funcionamiento del juego y piensa en los tipos que son necesarios. Realiza una lista durante el análisis visual y posteriormente implementa dichos TADs.

type Id = Int -- Ids de los elementos
type Health = Float -- Salud del robot
type Velocity = (Float, Float) -- Velocidad del robot y/o proyectil en x e y
type Damage = Float -- Daño que realiza el proyectil 
type HaveExploded = Bool -- Vemos si un robot ya ha explotado (lo hace cuando su vida llega a 0)
type Shoot = Float -- Consideramos esto como el cooldown en segundos
type TurretAction = Float -- Consideramos que es el angulo, en grados, en el que gira la torreta para apuntar
type Duration = Float


-- "Objetos" del mundo
data Projectile = Projectile { idP :: Id 
                         , vertexProjectile :: [Point] -- 4 vértices del proyectile formando el rectángulo de su forma
                         , positionP :: Position
                         , velocityP :: Velocity
                         , damageP :: Damage
                         , rangeP :: Distance
                         } deriving (Show, Eq)

data Turret = Turret { idT :: Id -- Que coincide con su robot asociado 
                   , vectorT :: Vector
                   , angleT :: Angle
                   , projectileT :: Projectile
                   , turretAction :: TurretAction
                    , shoot :: Shoot
                   } deriving (Show, Eq)

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Stop deriving (Show, Eq)

data Robot = Robot { idR :: Id
                   , vertexRobot :: [Point] -- 4 vértices del robot formando el rectángulo de su forma
                   , positionR :: Position -- Calculada a partir de los vértices
                   , velocityR :: Velocity
                   , healthR :: Health
                   , radarRange :: Distance
                   , turret :: Turret
                   , haveExploded :: HaveExploded
                   , damageR :: Damage -- Daño que realiza el robot al colisionar con otro
                   } deriving (Show, Eq)


data World = World { robots :: [Robot]
                   , projectiles :: [Projectile]
                   , turrets :: [Turret]
                   , robotHits :: [RobotHit] -- Lista de eventos de colisión detectados en el último frame
                    } deriving (Show, Eq)

data Explosion = Explosion { positionE :: Position
                           , sizeE :: Size
                           , durationE :: Duration
                           , robotHit :: RobotHit-- Calculado en la función de colisiones
                           } deriving (Show, Eq)

data RobotHit= RobotHitByProjectile { idRobot :: Id
                                    , idProjectile :: Id
                                    , damageHit :: Damage
                                    , hitAt :: Position
                                    } 
                                    | RobotCollidedWithRobot { idRobot1 :: Id
                                    , idRobot2 :: Id
                                    , damageHit1 :: Damage
                                    , damageHit2 :: Damage
                                    , hitAt :: Position
                                    } deriving (Show, Eq)

-- Lo usamos como velocidad base para cualquier robot, a la hora de hacer un cambio de dirección (updateVelocity)             
baseSpeed :: Float
baseSpeed = 5.0  


--1. Sistema de Detección de Colisiones


--Revisar el Separating Axis Theorem (SAT): Estudia el teorema del eje separador en: https://programmerart.weebly.com/separating-axis-theorem.html


--Implementar las siguientes funciones de colisión:


--checkCollision: Comprueba si dos rectángulos han colisionado utilizando el algoritmo apropiado.

checkCollision :: [Point] -> [Point] -> Bool
checkCollision rect1 rect2 = all (\axis -> superposicionPorEje rect1 rect2 axis) ejes
  where
    ejes = getAxes rect1 ++ getAxes rect2 -- Vectores perpendiculares a cada arista de ambos rectángulos (lista con 8 vectores)

    getAxes :: [Point] -> [Vector]                                                  -- [((p1,p2), (p3,p4)), ((p5,p6), (etc)), ((), ()), ((), ())]
    getAxes pts = [perp (sub p2 p1) | (p1, p2) <- zip pts (tail pts ++ [head pts])] -- [(A,B), (B,C), (C,D), (D,A)] forma las aristas del polígono
    -- Aqui ya tenemos los vectores perpendiculares a cada arista (lado del rectangulo)

    superposicionPorEje :: [Point] -> [Point] -> Vector -> Bool
    superposicionPorEje p1 p2 ejes =
        not (max1 < min2 || max2 < min1)
        where
            (min1, max1) = projectPolygon p1 ejes
            (min2, max2) = projectPolygon p2 ejes


    projectPolygon :: [Point] -> Vector -> (Float, Float)
    projectPolygon pts ejes =
        (minimum projections, maximum projections)
        where   
            projections = map (`dot` ejes) pts
      

--detectRobotProjectileCollisions: Verifica qué proyectiles han colisionado con algún agente. Cuando detecte una colisión, debe generar el evento de colisión correspondiente.

detectedRobotProjectileCollisions :: [Robot] -> [Projectile] -> ([RobotHit], Int)
detectedRobotProjectileCollisions robots proyectiles = (hits, total)
  where
    hits =
      [ RobotHit
          { robotIdHit      = idR r  
          , projectileIdHit = idP p
          , damageHit       = damageP p
          , hitAt           = positionP p
          }
      | r <- robots
      , p <- proyectiles
      , checkCollision (vertexRobot r) (vertexProjectile p)
      ]

    total = length hits

--detectRobotRobotCollisions: Comprueba y detecta las colisiones entre los diferentes robots del juego. Deberá generar el evento de colisión correspondiente.

detectRobotRobotCollisions :: [Robot] -> ([RobotHit], Int)
detectRobotRobotCollisions robots = (hits, total)
  where
    total = length hits
    hits =
  [RobotCollidedWithRobot
      { idRobot1       = idR r1
      , idRobot2       = idR r2
      , damageToRobot1 = d1
      , damageToRobot2 = d2
      }
  | r1 <- robots
  , r2 <- robots
  , idR r1 < idR r2   -- evita duplicados y self-pairs
  , checkCollision (vertexRobot r1) (vertexRobot r2)
  ]

--checkCollisions: Función principal que coordina todas las comprobaciones de colisión.

checkCollisions :: World -> Int
checkCollisions world = totalRP + totalRR
  where
    rs = robots world
    ps = projectiles world
    totalRP = length [ () | r <- rs, p <- ps
                          , checkCollision (vertexRobot r) (vertexProjectile p) ]
    totalRR = length [ () | r1 <- rs, r2 <- rs
                          , idR r1 < idR r2
                          , checkCollision (vertexRobot r1) (vertexRobot r2) ]


--2. Organización del Código en Módulos

--Revisar el tema de Módulos: Estudia cómo estructurar el código en módulos para mejorar la organización, mantenibilidad y reutilización del código.



--Reorganizar el código implementado: Divide tu implementación actual en módulos lógicos (por ejemplo: módulo de física, módulo de entidades, módulo de colisiones, módulo de renderizado, módulo de IA/comportamiento, etc.).



--3. Abstracción mediante Tipos Genéricos


--Crear tipo genérico base: Diseña un tipo de datos genérico que abstraiga las propiedades comunes de todos los objetos del juego (posición, velocidad, tamaño, etc.).


--Reimplementar tipos existentes: Refactoriza tus tipos de datos actuales para que hereden o utilicen este tipo genérico base.


--Actualizar funciones: Modifica las funciones implementadas previamente para que trabajen con el nuevo sistema de tipos genéricos, asegurando compatibilidad y consistencia.




--4. Sistema de Memoria para Agentes

--         Los agentes necesitan una memoria (implementada como diccionario) para tomar decisiones inteligentes. Crea un tipo de datos flexible que pueda almacenar diferentes tipos de información: enteros, cadenas de texto, puntos/coordenadas, booleanos, etc.


-- 5. DSL para acciones del Bot


-- Crea tipo para definir las acciones que se pueden indicar sobre un Bot.
-- Game/DSL.hs
module Game.DSL
  ( Direction(..)
  , Target(..)
  , BotCond(..)
  , BotCmd(..)
  ) where

import Game.Entities (Id, Position, Vector, Angle, Distance, Duration)
import Game.Memory   (MemoryValue)  -- para recordar cosas en la memoria del bot

-- Direcciones básicas (útil para órdenes rápidas)
data Direction
  = North | South | East | West
  deriving (Show, Eq)

-- A qué/Quién apuntamos o seguimos
data Target
  = TPoint Position    -- un punto del mundo
  | TRobot Id          -- otro robot por Id
  deriving (Show, Eq)

-- Condiciones sencillas para control de flujo del DSL
data BotCond
  = MemExists String               -- existe la clave en memoria
  | MemBool   String Bool          -- la clave es un Bool con ese valor
  | MemIntGE  String Int           -- la clave Int >= n
  | MemFloatLE String Float        -- la clave Float <= x
  | HealthBelow Float              -- la vida actual < x   (si tu runtime lo soporta)
  | EnemyInRange Distance          -- hay enemigo en rango (evaluado en el runtime)
  deriving (Show, Eq)

-- DSL de acciones del Bot
data BotCmd
  = MoveTo Position                  -- moverse a una posición absoluta
  | MoveBy Vector                    -- desplazarse relativo (delta x,y)
  | MoveDir Direction Distance       -- moverse en una dirección una distancia
  | StopMove                         -- parar movimiento

  | AimAt Target                     -- orientar torreta al objetivo
  | RotateTurret Angle               -- rotar torreta un ángulo
  | Shoot                            -- disparar al objetivo actual (si hay)
  | ShootAt Target                   -- disparar a un objetivo concreto

  | Wait Duration                    -- esperar (cooldowns, sincronización)

  | Remember String MemoryValue      -- escribir en memoria
  | Forget   String                  -- borrar clave de memoria

  | Follow Target                    -- seguir a un objetivo
  | Patrol  [Position]               -- patrullar por una serie de puntos

  -- Control de flujo del propio DSL (composición)
  | Seq    [BotCmd]                  -- secuencia de acciones
  | Repeat Int [BotCmd]              -- repetir n veces
  | If BotCond [BotCmd] [BotCmd]     -- condicional (then / else)
  deriving (Show, Eq)

-- Implementa un bot de ejemplo: Implementa una función que recibe la información sobre el estado del juego y devuelve un conjunto de acciones definidas por el tipo anterior.

Te dejo un bot de ejemplo que, dado el estado del juego, decide acciones usando el DSL del ejercicio 5:

Si hay enemigo en rango, apunta y dispara, y recuerda su última posición.

Si no hay enemigo visible pero recuerda una posición anterior, se mueve hacia allí (y olvida el recuerdo cuando llega).

Si no tiene nada, se para.

-- Game/Bot.hs
module Game.Bot
  ( decideBot ) where

import Game.Entities  (World(..), Robot(..), Id, Position, Distance)
import Game.DSL       (BotCmd(..), Direction(..), Target(..), BotCond(..))
import Game.Memory    (MemoryValue(..), getMemory)
import Data.Maybe     (listToMaybe)
import Data.List      (minimumBy)
import Data.Ord       (comparing)

-- | Política del bot: dado el mundo y "yo" (robot), devuelve una lista de acciones.
decideBot :: World -> Robot -> [BotCmd]
decideBot world me =
  case nearestEnemyInRange me (robots world) of
    -- 1) Enemigo visible y en rango: recuerda su posición, apunta y dispara
    Just enemy ->
      [ Remember "lastEnemy" (MemPoint (positionR enemy))
      , AimAt (TRobot (idR enemy))
      , Shoot
      ]

    -- 2) Sin enemigo visible, pero recuerdo previo: ve hacia allí; si llegó, olvida
    Nothing ->
      case getMemory "lastEnemy" (memory me) of
        Just (MemPoint pos) ->
          if dist (positionR me) pos <= arriveEpsilon
            then [ Forget "lastEnemy", StopMove ]
            else [ MoveTo pos ]
        _ -> [ StopMove ]  -- 3) Nada que hacer

  where
    arriveEpsilon :: Distance
    arriveEpsilon = 1.5

-- ========= Helpers =========

-- Enemigo más cercano DENTRO del radar (excluyendo a "me")
nearestEnemyInRange :: Robot -> [Robot] -> Maybe Robot
nearestEnemyInRange me rs =
  let others   = filter ((/= idR me) . idR) rs
      inRange  = filter (\r -> dist (positionR me) (positionR r) <= radarRange me) others
  in case inRange of
       [] -> Nothing
       _  -> Just (minimumBy (comparing (dist (positionR me) . positionR)) inRange)

-- Distancia euclídea entre posiciones
dist :: Position -> Position -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)