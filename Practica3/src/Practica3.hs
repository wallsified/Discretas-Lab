{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

type EList a = [(a,a)]ll
pruebaElist =  [(1,2),(3,4),(5,6)]
pruebaElist2 =  [(11,22),(33,44),(55,66),(77,88)]

-- Dada una EList obtiene su número de elementos.
longP :: EList a -> Int
longP [] = 0
longP ((a, b) : xs) = 2 + longP xs

-- Función que realiza la concatenación de dos EList
appendP :: EList a -> EList a -> EList a
appendP [] l = l
appendP ((a, b) : xs) l = (a, b) : appendP xs l

-- Función que agrega dos elementos al final de una EList.
snocP :: (a, a) -> EList a -> EList a
snocP (a,b) [] = [(a, b)]git 
snocP (a, b) ((x, y) : xs) = (x, y) : snocP (a, b) xs

    -- Elabora una función que haga la reversa de una EList. -}
    reversaP :: EList a -> EList a
    reversaP [] = []
    reversaP ((x, y) : xs) = snocP (y, x) (reversaP xs)
