import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color 
-- EJEMPLO 1 : Dibujo estático de figuras y trasformaciones
            -- Dibujo un rectangulo, lo Lo giro y luego lo muevo.

ejemplo1 = display ventana fondo imagen
  where
    ventana = InWindow "Ejemplo 1: Dibujo con transformaciones" (600, 600) (100, 100)
    fondo   = white
    imagen  = Pictures
      [ -- Rectángulo original
        Color rose (rectangleWire 80 120),

        -- Rectángulo rotado 45°
        Color violet (Rotate 45 (rectangleWire 80 120)),

        -- Rectángulo rotado y luego trasladado hacia la derecha
        Color blue (Translate 150 0 (Rotate 45 (rectangleWire 80 120))),

        -- Textos explicativos
        Translate (-200) 250 $ Scale 0.15 0.15 $ Color rose (Text "Original"),
        Translate (-20) 250  $ Scale 0.15 0.15 $ Color violet (Text "Rotado 45°"),
        Translate (130) 250  $ Scale 0.15 0.15 $ Color blue (Text "Rotado + Trasladado")
      ]
    --}

-- EJEMPLO 2 : Animación básica : dibujo con movimiento continuo
--Dibujo un reloj: un círculo estñatico con una flecha que gira
ejemplo2 = animate ventana white dibujo
    where 
        ventana = InWindow "Ejemplo 2: Animación"
                            (400,400)(10, 10)
        dibujo :: Float -> Picture
        -- El argumento Float es el tiempo (en segundos) desde que empezó la animación
        dibujo tiempo = Pictures
            [ Color black (circle 150), -- círculo estático (el reloj)
              Color red (Rotate (-tiempo * 50) flecha), -- Flecha que gira con el tiempo
                                                        -- tiempo negativo porque Gloss gira en sentido antihorario
              Color black (circleSolid 5) -- Punto central
            ]

        -- Definición de la flecha (un triángulo alargado)
        flecha :: Picture
        flecha = Polygon [ (0,0)   -- centro (base)
                         , (-8,20) -- lado izquierdo de la base
                         , (0,120) -- punta de la flecha
                         , (8,20)  -- lado derecho de la base
                        ]
--}

--EJEMPLO 3 : Interacción con teclado 
--Dibujo un helado (un poligono y tres circulos) y lo muevo con teclado
ejemplo3 = play ventana white 60 (0, 0)
        dibujar manejarEvento actualizar
    where
        ventana = InWindow "Mover helado con teclado" (400, 400) (10, 10)
        
        -- Dibuja el helado en la posición actual
        dibujar (x, y) = Translate x y helado

        manejarEvento evento (x, y) = case evento of
            EventKey (SpecialKey KeyLeft) Down _ _ ->
                (x - 10, y)
            EventKey (SpecialKey KeyRight) Down _ _ ->
                (x + 10, y)
            EventKey (SpecialKey KeyUp) Down _ _ ->
                (x, y + 10)
            EventKey (SpecialKey KeyDown) Down _ _ ->
                (x, y - 10)
            _ -> (x, y)

        actualizar _ estado = estado

        -- Definición de la figura de corazón
        helado :: Picture
        helado = Pictures
            [ -- Parte superior: tres círculos
                Translate 0 35 $ Color yellow (circleSolid 40),
                Translate (-35) (-27) $ Color (makeColor 0.4 0.2 0.2 1.0) (circleSolid 40),
                Translate 35 (-27)  $ Color (makeColor 1.0 0.0 0.8 0.3) (circleSolid 40),
            
            -- Cono (triángulo)
                Color (makeColor 1.0 0.7 0.1 0.3) $ Polygon
                [ (-40, -60)   
                , (40, -60)    
                , (0, -200)    
                ]
            ]

main :: IO ()
main = do
  putStrLn "Selecciona un ejemplo de Gloss:"
  putStrLn "1 = Dibujo  |  2 = Animación  |  3 = Interactivo"
  opcion <- getLine
  case opcion of
    "1" -> ejemplo1
    "2" -> ejemplo2
    "3" -> ejemplo3
    _   -> putStrLn "Opción no válida."
