module Library where
import PdePreludat

type Nombre = String
type Antiguedad = Number
type Grado = Number
type Clan = String

data Hechicero = UnHechicero {
    antiguedad :: Antiguedad, 
    grado :: Grado,
    clan :: Clan
} deriving Show

nobara :: Hechicero
nobara = UnHechicero 1 3 "Kugisaki" 

satoru :: Hechicero
satoru = UnHechicero 15 0 "Gojo"

maki :: Hechicero
maki = UnHechicero 3 4 "Zenin" 

yuji :: Hechicero 
yuji = UnHechicero 0 1 "Itadori" 

clanesPrestigiosos = ["Zenin", "Kamo", "Gojo"]

grupoA :: [Hechicero]
grupoA = [nobara, yuji, satoru, maki]

estaPreparado :: [Hechicero] -> Bool
estaPreparado = (>3).length 

esInvencible :: [Hechicero] -> Bool
esInvencible hechiceros = any esEspecial hechiceros

esEspecial :: Hechicero -> Bool
esEspecial = (==0).grado

esPrestigioso :: Hechicero -> Bool
esPrestigioso hechicero = elem (clan hechicero) clanesPrestigiosos

esFavorito :: [Hechicero] -> Bool
esFavorito hechiceros = all esPrestigioso hechiceros

esExperto :: Hechicero -> Bool
esExperto = (>1).antiguedad

sonExpertos :: [Hechicero] -> [Hechicero]
sonExpertos hechiceros = filter esExperto hechiceros

subirGrado :: Hechicero -> Hechicero
subirGrado hechicero
    | grado hechicero > 0 = hechicero { grado = grado hechicero - 1 }
    | otherwise = hechicero { grado = 0 }

hacerFrente :: [Hechicero] -> Bool
hacerFrente hechiceros = esInvencible hechiceros || estaPreparado hechiceros

powerUp :: [Hechicero] -> [Hechicero]
powerUp = map subirGrado

nivelTryhard :: Hechicero -> Number
nivelTryhard = (/1).(+1).grado

misionDificil :: Hechicero -> Hechicero -> Hechicero
misionDificil hechi1 hechi2 
    | nivelTryhard hechi1 >= nivelTryhard hechi2 = hechi1
    | otherwise = hechi2

nivelBurocratico :: Hechicero -> Number
nivelBurocratico = length.clan

mayorLetra :: String -> Char
mayorLetra = maximum

nivelIntimidante :: Hechicero -> Hechicero -> Hechicero
nivelIntimidante hechi1 hechi2
    | mayorLetra (clan hechi1) >= mayorLetra (clan hechi2) = hechi1
    | otherwise = hechi2

nivelSigilo :: Hechicero -> Number
nivelSigilo = (*6).antiguedad



