-- Apartado 5 de HAUS3 (Simplemente hay que indicar qué instrucciones van a poder recibir nuestros bots)

module BotAction
  ( Target(..)
  , BotCond(..)
  , BotCmd(..)
  ) where

import Utils 
import Memory     -- para la memoria del bot
import Types    
import Movement                      -- funciones de movimiento: detectedAgent, isRobotAlive, updateVelocity, etc.

-- A qué/Quién apuntamos o seguimos
data Target
  = TPoint Position    -- un punto del mundo
  | TRobot Id          -- otro robot por Id
  deriving (Show, Eq)

-- Condiciones que puede evaluar el bot
data BotCond
  = MemExists String               -- existe la clave en memoria
  | MemBool   String Bool          -- la clave es un Bool con ese valor
  | MemIntGE  String Int           -- la clave Int >= n
  | MemFloatLE String Float        -- la clave Float <= x
  | HealthBelow Float              -- usa 'isRobotAlive' y 'healthR' del robot
  | EnemyInRange Distance          -- usa 'detectedAgent' y 'distanceBetween'
  deriving (Show, Eq)

-- Comandos que puede ejecutar el bot
data BotCmd
  = MoveTo Position                  -- moverse a una posición absoluta
  | MoveBy Vector                    -- desplazarse relativo (delta x,y)
  | MoveDir Action Distance          -- moverse en una dirección (usa 'updateVelocity')
  | StopMove                         -- parar movimiento

  | AimAt Target                     -- orientar torreta al objetivo (usa 'angleToTarget')
  | RotateTurret Angle               -- rotar torreta un ángulo
  | Shoot                            -- disparar al objetivo actual
  | ShootAt Target                   -- disparar a un objetivo concreto (usa 'angleToTarget' y 'detectedAgent')

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
