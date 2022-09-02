{-
Estructuras Discretas 2023-1
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

myMod :: Int -> Int -> Int --Auxiliar para par e inpar. Implementa mod nativo de Haskell. 
myMod n m = if (n < m) then n else myMod (n-m) m

par :: Int -> Bool  -- Si el residuo es 0, entonces es par (incluyendo el 0)
par n = (myMod n 2) == 0

inpar :: Int -> Bool -- Si el residuo es 1, entonces es inpar.
inpar n = (myMod n 2) == 1

minimo :: Int -> Int -> Int -- Usamos un if y las comparaciones lógicas
minimo x y = if (x<=y) then x else y

maximo :: Int -> Int -> Int -- Reverse minimo.
maximo x y = if (x>=y) then x else y

absolutN :: Int -> Int
absolutN x = if (x<0) then -x else x --Esta trae truco. Haskell no detecta negativos como -x, pero si como (-x)

diveE :: Int -> Int -> Int 
diveE x y = helpDive x y 0 -- el último 0 es un indicador de cuantas veces se repite. 

-- Función auxiliar para diveE, la vimos en clase como divEAux
helpDive :: Int -> Int -> Int -> Int
helpDive x y counter = if (x<y) then counter else helpDive (x-y) y (counter+1) 

cabeza :: [Int] -> Int -- Devolvemos la lista con x siendo la cabeza (como elemento)
cabeza (x:xs) = x

cola ::  [Int] -> [Int] -- Devolvemos la lista con los elementos xs como una nueva lista. 
cola (x:xs) = xs

quita :: Int -> [Int] -> [Int] -- Quitamos x elementos de una lista y regresamos la lista - x elementos. 
quita 0 (x:xs) = (x:xs) -- Caso Base
quita n (x:xs) = quita (n-1) xs -- Caso Recursivo

enesimo :: Int -> [Int] -> Int --Según encontré, el usar !! en Haskell es justo la idea de nuestra función. Al !! se le llama "hoogle"
enesimo x xs = xs!! x --Entonces es un poco la idea decirle que de xs tome x y regrese la lista sin x