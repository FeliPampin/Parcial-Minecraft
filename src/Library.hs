{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import Data.List (intersect)

doble :: Number -> Number
doble numero = numero + numero

type Material = String

data Personaje = UnPersonaje {
        nombre :: String,
        puntaje :: Number,
        inventario :: [Material]
} deriving Show

data Receta = UnaReceta {
    nombreReceta :: String,
    materiales :: [Material],
    tiempo :: Number

} deriving Show


recetaFogata :: Receta
recetaFogata = UnaReceta fogata [madera, fosforo] 10

madera :: Material
madera = "Madera"

fosforo :: Material
fosforo = "Fosforo"

recetaPolloAsado :: Receta
recetaPolloAsado = UnaReceta polloAsado [fogata, pollo] 300

fogata :: Material
fogata = "Fogata"

pollo :: Material
pollo = "Pollo"

polloAsado :: Material
polloAsado = "Pollo Asado"

recetaSueter :: Receta
recetaSueter = UnaReceta sueter [lana, agujas, tintura] 600

lana :: Material
lana = "lana"

agujas :: Material
agujas = "agujas"

tintura :: Material
tintura = "tintura"

sueter :: Material
sueter = "sueter"

steve :: Personaje
steve = UnPersonaje "Steve" 100 [sueter, lana]

listaRecetas1 :: [Receta]
listaRecetas1 = [recetaFogata, recetaPolloAsado, recetaSueter]

-- Craft

intentarCraftear :: Personaje -> Receta -> Personaje
intentarCraftear personaje receta
    | intersect (materiales receta) (inventario personaje) == materiales receta = craftear receta personaje
    | otherwise = personaje {puntaje = puntaje personaje - 100}

craftear :: Receta -> Personaje -> Personaje
craftear receta = cambiarPuntaje receta . agregarElementoAInventario (nombreReceta receta) . eliminarMaterialesDeInventario receta

eliminarMaterialesDeInventario :: Receta -> Personaje -> Personaje
eliminarMaterialesDeInventario receta personaje = personaje {inventario = eliminarMaterialesDeInventarioAux (materiales receta) personaje}

eliminarMaterialesDeInventarioAux :: [Material] -> Personaje -> [Material]
eliminarMaterialesDeInventarioAux [] personaje = inventario personaje
eliminarMaterialesDeInventarioAux (elemento:siguiente) personaje = eliminarMaterialesDeInventarioAux siguiente (verificarSiEliminar elemento personaje)

agregarElementoAInventario :: Material -> Personaje -> Personaje
agregarElementoAInventario material personaje = personaje {inventario = material : inventario personaje}

cambiarPuntaje :: Receta -> Personaje -> Personaje
cambiarPuntaje receta personaje = personaje {puntaje = puntaje personaje + (tiempo receta* 10)}

verificarSiEliminar :: Material -> Personaje -> Personaje
verificarSiEliminar material personaje = personaje {inventario = verificarSiEliminarAux material (inventario personaje)}

verificarSiEliminarAux :: Material -> [Material] -> [Material]
verificarSiEliminarAux _ [] = []
verificarSiEliminarAux elemento (elem:siguiente)
    | elem == elemento = verificarSiEliminarAux elemento siguiente
    | otherwise = elem : verificarSiEliminarAux elemento siguiente

cualesPuedeCraftear :: Personaje -> [Receta] -> [Receta]
cualesPuedeCraftear personaje = filter (encontrarPosiblesRecetas personaje)

encontrarPosiblesRecetas :: Personaje -> Receta -> Bool
encontrarPosiblesRecetas personaje receta = verificarSiPuedeCraftear personaje receta && verificarSiDuplicaPuntaje personaje receta

verificarSiDuplicaPuntaje :: Personaje -> Receta -> Bool
verificarSiDuplicaPuntaje personaje receta = puntaje (intentarCraftear personaje receta) >= puntaje personaje * 2

verificarSiPuedeCraftear :: Personaje -> Receta -> Bool
verificarSiPuedeCraftear personaje receta = inventario (intentarCraftear personaje receta) /= inventario personaje

aplicarRecetasSucesivamente :: Personaje -> [Receta] -> Personaje
aplicarRecetasSucesivamente = foldl intentarCraftear

aplicarRecetasSucesivamenteAlReves :: Personaje -> [Receta] -> Personaje
aplicarRecetasSucesivamenteAlReves = foldr (flip intentarCraftear)

quedaConMasPuntosEnOrden :: Personaje -> [Receta] -> Bool
quedaConMasPuntosEnOrden personaje recetas = puntaje (aplicarRecetasSucesivamente personaje recetas) > puntaje (aplicarRecetasSucesivamenteAlReves personaje recetas)

quedaConMasPuntosAlReves :: Personaje -> [Receta] -> Bool
quedaConMasPuntosAlReves personaje recetas = puntaje (aplicarRecetasSucesivamente personaje recetas) < puntaje (aplicarRecetasSucesivamenteAlReves personaje recetas)

-- Mine

data Bioma = UnBioma {
    nombreBioma :: String,
    elementos :: [Material],
    elementoNecesario :: Material
} deriving Show

type Herramienta = [Material] -> Number -> Material

hielo :: Material
hielo = "Hielo"

iglues :: Material
iglues = "Iglues"

lobos :: Material
lobos = "lobos"

artico :: Bioma
artico = UnBioma "Artico" [hielo, iglues, lobos] sueter

desierto :: Bioma
desierto = UnBioma "Desierto" [arena, sal, agua, mar, olas] sueter

arena, sal, agua, mar, olas :: Material
arena   = "Arena"
sal     = "Sal"
agua    = "agua"
mar     = "mar"
olas    = "olas"

hacha :: Herramienta
hacha materiales _ = last materiales

espada :: Herramienta
espada materiales _ = head materiales

pico :: Herramienta
pico = (!!)

intentarMinar :: Personaje -> Bioma -> Herramienta -> Number -> Personaje
intentarMinar personaje bioma herramienta num
    | cuentaConElementos personaje bioma = minar personaje bioma herramienta num
    | otherwise = personaje

cuentaConElementos :: Personaje -> Bioma -> Bool
cuentaConElementos personaje bioma = any (esIgualAElementoNecesario bioma) (inventario personaje)

esIgualAElementoNecesario :: Bioma -> Material -> Bool
esIgualAElementoNecesario bioma material = elementoNecesario bioma == material

minar :: Personaje -> Bioma -> ([Material] -> Number -> Material) -> Number -> Personaje
minar personaje bioma herramienta num = sumarPuntos . flip agregarElementoAInventario personaje . obtenerMaterialMinado bioma herramienta $ num

obtenerMaterialMinado :: Bioma -> ([Material] -> Number -> Material) -> Number -> Material
obtenerMaterialMinado bioma herramienta num = herramienta (elementos bioma) num

sumarPuntos :: Personaje -> Personaje
sumarPuntos personaje = personaje {puntaje = puntaje personaje + 50}

-- Herramientas punto 2

pala :: Herramienta
pala materiales _ = pico materiales . (`div` 2) $ length materiales

--manos :: Herramienta
--manos materiales _ = \x -> 

devolverBioma :: Bioma -> Bioma 
devolverBioma bioma = bioma {elementos = agregarMaterialABioma agua}

agregarMaterialABioma :: Material -> [Material]
agregarMaterialABioma material = material : agregarMaterialABioma material