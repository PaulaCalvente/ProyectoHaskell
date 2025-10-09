import Graphics.Gloss

main :: IO ()
main = display ventana white escenaCompleta
  where
    ventana = InWindow "Figura pixel art" (600, 600) (100, 100)

    -- Dibujo total (escena + personaje)
    escenaCompleta = Pictures [escena, Translate 0 (-100) dibujo]

    -- Escena de fondo
    escena = Pictures
      [ cielo
      , sol
      , casa
      , suelo
      ]
      where
        cielo = Color (makeColor 0 0 0.5 1) $
          Polygon [(-400,-300), (400,-300), (400,300), (-400,300)]
        sol = Translate 200 200 $ Color white $ circleSolid 2
        casa = Pictures
          [ Color red $ Polygon [(-50,0), (50,0), (0,70)]
          , Color white $ Translate 0 (-50) (rectangleSolid 100 100)
          ]
        suelo = Color green $ Translate 0 (-150) $
          Polygon [(-400,0), (400,0), (400,-300), (-400,-300)]

    -- Dibujo del personaje
    dibujo = Pictures
      [ Color (makeColorI 170 210 90 255) $ Polygon [(-40,60),(40,60),(40,20),(-40,20)]
      , Color (makeColorI 170 210 90 255) $ Polygon [(-40,60),(-80,40),(-40,40)]
      , Color (makeColorI 170 210 90 255) $ Polygon [(40,60),(80,40),(40,40)]
      , Color (makeColorI 100 160 255 255) $ Translate 0 (-30) (rectangleSolid 100 100)
      , Color (makeColorI 0 0 40 255) $
          Line [(-80,40),(-40,60),(40,60),(80,40),(40,40),(50,20),(0,-60),(-50,20),(-40,40),(-80,40)]
      , Color black $ Translate (-15) 40 (circleSolid 5)
      , Color black $ Translate (15) 40 (circleSolid 5)
      ]
