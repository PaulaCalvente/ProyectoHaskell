module Data.DatosComunes where

--Tipos básicos en el juego
type Point = (Float, Float)
type Vector = (Float, Float)
type Angle = Float
type Distance = Float
type Position = (Float, Float)
type Size = (Float, Float)

type Id = Int
type Health = Float
type Velocity = (Float, Float)
type Damage = Float
type HaveExploded = Bool
type Cooldown = Float
type TurretAction = Float
type Duration = Float

-- CommonData: datos comunes para robots y proyectiles 
data CommonData a = CommonData
  { id       :: Id
  , damage   :: Damage
  , position :: (a, a)   -- posición como coordenadas (x, y)
  , velocity :: (a, a)   -- velocidad también como vector (vx, vy)
  , size     :: Size
  , points   :: [(a, a)] -- lista de puntos
  } deriving (Show, Eq)

-- FUNCTOR: aplica una función a todos los campos numéricos.
instance Functor CommonData where
  fmap f (CommonData i d (px, py) (vx, vy) s pts) =
    CommonData i d (f px, f py) (f vx, f vy) s (map (\(x, y) -> (f x, f y)) pts)

instance Applicative CommonData where
  pure x = CommonData 0 0 (x, x) (x, x) (0, 0) [(x, x)]
  (CommonData i d (fx, fy) (fvx, fvy) s _) <*> (CommonData _ _ (px, py) (vx, vy) _ _) =
    CommonData i d (fx px, fy py) (fvx vx, fvy vy) s []