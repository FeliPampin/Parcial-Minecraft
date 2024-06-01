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

}


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
steve = UnPersonaje "Steve" 100 [madera, fosforo, pollo]

intentarCraftear :: Receta -> Personaje -> Personaje
intentarCraftear receta personaje
    | intersect (materiales receta) (inventario personaje) == materiales receta = craftear receta personaje
    | otherwise = personaje {puntaje = puntaje personaje - 100}

craftear :: Receta -> Personaje -> Personaje
craftear receta = cambiarPuntaje receta . agregarElementoCrafteado (nombreReceta receta) . eliminarMaterialesDeInventario receta

cambiarPuntaje :: Receta -> Personaje -> Personaje
cambiarPuntaje receta personaje = personaje {puntaje = puntaje personaje + (tiempo receta* 10)}

eliminarMaterialesDeInventario :: Receta -> Personaje -> Personaje
eliminarMaterialesDeInventario receta personaje = personaje {inventario = eliminarMaterialesDeInventarioAux (materiales receta) personaje}

eliminarMaterialesDeInventarioAux :: [Material] -> Personaje -> [Material]
eliminarMaterialesDeInventarioAux [] personaje = inventario personaje
eliminarMaterialesDeInventarioAux (elemento:siguiente) personaje = eliminarMaterialesDeInventarioAux siguiente (verificarSiEliminar elemento personaje)

verificarSiEliminar :: Material -> Personaje -> Personaje
verificarSiEliminar material personaje = personaje {inventario = verificarSiEliminarAux material (inventario personaje)}

verificarSiEliminarAux :: Material -> [Material] -> [Material]
verificarSiEliminarAux _ [] = []
verificarSiEliminarAux elemento (elem:siguiente)
    | elem == elemento = verificarSiEliminarAux elemento siguiente
    | otherwise = elem : verificarSiEliminarAux elemento siguiente

agregarElementoCrafteado :: Material -> Personaje -> Personaje
agregarElementoCrafteado material personaje = personaje {inventario = material : inventario personaje}