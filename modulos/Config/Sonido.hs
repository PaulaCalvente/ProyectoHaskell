module Config.Sonido (iniciarMusica, detenerMusica) where

import qualified SDL
import qualified SDL.Mixer as Mixer

-- Inicia SDL2 y reproduce música en bucle
iniciarMusica :: FilePath -> IO ()
iniciarMusica archivo = do
    SDL.initialize [SDL.InitAudio]         -- Inicializa audio
    Mixer.openAudio Mixer.defaultAudio 256 -- Abre audio
    -- No hace falta allocateChannels
    musica <- Mixer.load archivo
    Mixer.playMusic Mixer.Forever musica   -- Reproduce música en bucle
    return ()

-- Detiene la música y limpia recursos
detenerMusica :: IO ()
detenerMusica = do
    Mixer.closeAudio
    SDL.quit
