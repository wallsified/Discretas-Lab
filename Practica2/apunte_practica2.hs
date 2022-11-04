-- Clases 01, 09/Septiembre/2022

-- Función para calcular nuestra edad en otros planetas de nuestro sistema solar.
data Planeta = Tierra | Mercurio | Venus | Marte | Jupiter | Saturno | Urano | Neptuno deriving (Show, Eq)

-- Show sirve para imprimir cosas "como son" en Haskell (por ejemplo, en este caso, los planetas se imprimen con su string).
-- EQ sirve para comparar entre ellas. Como son "clases" en Haskell, tanto EQ como Show "derivan" de ellas.
-- Por ejemplo en el caso de Planeta, derivan de Show para mostrarse en la terminal y de EQ para saber que son iguales
-- entre si.


-- instance Show EA where
   -- N n = n
    -- (Suma a b) = Show a ++ "+" ++ Show b 


-- Equivalencias de 1 año terrestre a otros planetas.
equivalencia :: Planeta -> Float
equivalencia Tierra   = 0
equivalencia Mercurio = 0.2
equivalencia Venus    = 0.61
equivalencia Marte    = 1.8
equivalencia Jupiter  = 11.86
equivalencia Saturno  = 29.44
equivalencia Urano    = 84.01
equivalencia Neptuno  = 164.7

edadEn :: Float -> Planeta -> Float
edadEn n p = n / (equivalencia p)

-- En Haskell una firma con parentesis es una firma a recibir.
-- Función map "mapear" Pasa una función a una lista de argumentos y regresa otra lista de argumentos
-- map (a -> b) -> [a] -> [b]
-- Función "filter" Filtra "algo" apartir de una condición y devuelve la lista de ese "algo" que cumpla la condición
-- filter (\x -> not (even x)) [1,2,3,4,5,6] da como resultado [1,3,5]
-- Ambas funciones son de orden superior, ya que toman como parámetros otra función para su funcionamiento. q
