module Library where
import PdePreludat

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- PUNTO 1

--a
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{
    velocidad = 10,
    precision = 2* precisionJugador habilidad,
    altura = 0
    }

madera :: Palo
madera habilidad = UnTiro{
    velocidad = 100,
    precision = 0.5* precisionJugador habilidad,
    altura = 5
    }

hierros :: Number -> Palo
hierros n habilidad = UnTiro{
    velocidad = fuerzaJugador habilidad * n,
    precision = div (precisionJugador habilidad) n,
    altura = max (n - 3) 0
    }

--b

palos :: [Palo]
palos = [putter,madera] ++ map hierros [1..10]

--PUNTO 2

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

--PUNTO 3

data Obstaculo = UnObstaculo {
    superaObstaculo :: Tiro -> Bool,
    efectosObstaculo :: Tiro -> Tiro
    }

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiro
                            | (superaObstaculo obstaculo) tiro = (efectosObstaculo obstaculo) tiro
                            | otherwise = tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

--a

tunel :: Obstaculo
tunel = UnObstaculo superaTunel efectosTunel

superaTunel :: Tiro -> Bool
superaTunel tiro = precision tiro > 90 && vaAlRasDelSuelo tiro
vaAlRasDelSuelo = (== 0).altura

efectosTunel :: Tiro -> Tiro
efectosTunel tiro = UnTiro{precision = 100,
                      altura = 0,
                      velocidad = velocidad tiro
                      }

--b

laguna :: Number -> Obstaculo
laguna largoLaguna = UnObstaculo superaLaguna (efectosLaguna largoLaguna)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = precision tiro > 80 && (between 1 5 . altura) tiro

efectosLaguna :: Number -> Tiro -> Tiro
efectosLaguna largoLaguna tiro = UnTiro{precision = precision tiro,
                       altura = div (altura tiro) largoLaguna,
                       velocidad = velocidad tiro
                       }

--c

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectosHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro && precision tiro > 95

efectosHoyo :: Tiro -> Tiro
efectosHoyo _ = tiroDetenido 

--PUNTO 4

--a

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (permiteSuperarObstaculo jugador obstaculo) palos

permiteSuperarObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
permiteSuperarObstaculo jugador obstaculo palo = (superaObstaculo obstaculo) ((palo.habilidad) jugador)

--b

cuantosConsecutivosPuedeSuperar :: [Obstaculo] -> Tiro -> Number
cuantosConsecutivosPuedeSuperar [] tiro = 0
cuantosConsecutivosPuedeSuperar (obstaculo : obstaculos) tiro
        | (superaObstaculo obstaculo) tiro = 1 + cuantosConsecutivosPuedeSuperar obstaculos (efectosObstaculo obstaculo tiro)
        | otherwise = 0

--c

--paloMasUtil :: Jugador -> [Obstaculo] -> Palo
--paloMasUtil jugador obstaculos = foldl (>) (habilidad jugador) obstaculos
