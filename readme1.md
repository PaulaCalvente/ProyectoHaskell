detectRobotRobotCollisions :: [Robot] -> [RobotHit]
detectRobotRobotCollisions robots =
  [ RobotCollidedWithRobot
      { idRobot1       = idR r1
      , idRobot2       = idR r2
      , damageToRobot1 = d1
      , damageToRobot2 = d2
      }
  | r1 <- robots
  , r2 <- robots
  , idR r1 < idR r2   -- evita duplicados y self-pairs
  , checkCollision (vertexRobot r1) (vertexRobot r2)
  , let (d1, d2) = collisionDamage r1 r2
  ]
  where
    -- Modelo simple de daño: proporcional a la velocidad relativa
    collisionDamage :: Robot -> Robot -> (Damage, Damage)
    collisionDamage a b =
      let (vx1,vy1) = velocityR a
          (vx2,vy2) = velocityR b
          speed1 = sqrt (vx1*vx1 + vy1*vy1)
          speed2 = sqrt (vx2*vx2 + vy2*vy2)
          rel    = sqrt ((vx1 - vx2)^2 + (vy1 - vy2)^2) -- velocidad relativa
          k      = 1.0 -- constante ajustable de “dureza” del impacto
          denom  = speed1 + speed2
          (w1, w2) = if denom > 0 then (speed2/denom, speed1/denom) else (0,0)
      in (k * rel * w1, k * rel * w2)
  
  ---------------------------
  




-------------------------
checkCollisions :: World -> Int
checkCollisions world =
    length [ () 
           | r <- robots world
           , p <- projectiles world
           , checkCollision (vertexRobot r) (vertexProjectile p)
           ]
  + length [ () 
           | r1 <- robots world
           , r2 <- robots world
           , idR r1 < idR r2
           , checkCollision (vertexRobot r1) (vertexRobot r2)
           ]



Primera lista por comprensión:

Recorre cada (robot, proyectil)

Comprueba si chocan con checkCollision

Si chocan, mete un () en la lista

length cuenta cuántos hay

Segunda lista por comprensión:

Recorre cada par de robots (r1, r2)

Usa idR r1 < idR r2 para no duplicar ni compararse consigo mismo

Comprueba colisión

Si chocan, mete un ()

length cuenta cuántos hay

Suma ambos números y lo devuelve.

----------------------------------------------------------------------
