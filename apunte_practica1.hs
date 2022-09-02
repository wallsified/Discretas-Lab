{-
Estructuras Discretas 2023-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: María Fernanda Mendoza Castillo
Laboratorio: Juan Alfonso Garduño Solís
-}

--Funcion que suma dos números.
zuma :: Int -> Int -> Int
zuma a b = a + b 

--Funcion que suma dos números pero aprovecha la propiedad de neutro aditivo y
--la caza de patrones de Haskell.
zuma2 :: Int -> Int -> Int
zuma2 a 0 = a
zuma2 0 b = b
zuma2 a b = a + b

--Funcion que responde si un número representa un año bisiesto
bisiesto :: Int -> Bool
bisiesto n = ((mod n 4) == 0 && (not (mod n 100 == 0 ))) || ((mod n 400) == 0)
    
--Funcion que revursivamente devuelve el factorial de un numero
fac :: Int -> Int       -- fac recibe un entero y devuelve otro entero
fac 1 = 1               -- Caso base
fac n = n * (fac (n-1)) -- Caso recursivo


lon :: [a] -> Int
lon []     = 0 -- Caso base, la longitud de una lista vacia es cero.
lon (x:xs) = 1 + lon xs -- Caso inductivo, la longitud
                                  -- de una lista con cabeza y cola es
                                  -- 1 mas la longitud de la cola.


--Funcion que suma todos los elementos de una lista de numeros.
f1 :: [Int] -> Int
f1 [] = 0
f1 (x:xs) = x + f1 xs


--Funcion que multiplica todos los elementos de una lista de numeros pero
-- SIEMPRE da 0, ¿por que?
f2 :: [Int] -> Int
f2 [] = 0
f2 (x:xs) = x * f2 xs

--Funcion que recibe una n y una m para devolver el equivalente a mod n m.
myMod :: Int -> Int -> Int
myMod n m = if (n < m)
            then n             -- Caso base
            else myMod (n-m) m -- Caso recursivo