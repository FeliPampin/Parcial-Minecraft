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
craftear receta personaje = cambiarPuntaje (agregarElementoCrafteado (nombreReceta receta) (eliminarMaterialesDeInventario (materiales receta) (inventario personaje)) personaje) receta

cambiarPuntaje :: Personaje -> Receta -> Personaje
cambiarPuntaje personaje receta = personaje {puntaje = puntaje personaje + ((tiempo receta)* 10)}

eliminarMaterialesDeInventario :: [Material] -> [Material] -> [Material]
eliminarMaterialesDeInventario [] inventarios = inventarios
eliminarMaterialesDeInventario (elemento:siguiente) inventarios = eliminarMaterialesDeInventario (verificarSiEliminar elemento inventarios) siguiente

verificarSiEliminar :: Material -> [Material] -> [Material]
verificarSiEliminar _ [] = []
verificarSiEliminar elemento (elem:siguiente)
    | elem == elemento = verificarSiEliminar elemento siguiente
    | otherwise = elem : verificarSiEliminar elemento siguiente

agregarElementoCrafteado :: Material -> [Material] -> Personaje -> Personaje
agregarElementoCrafteado elemento inventarios personaje = personaje {inventario = elemento : inventarios}