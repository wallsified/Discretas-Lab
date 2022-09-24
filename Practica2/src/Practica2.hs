{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

-- Algunas listas de prueba
pruebaNum = [0,1,2,3,4,5,6,7,8,9]
pruebaChar = ['a','b','c','d','e']
pruebaString = ["hola", "mundo", "this", "is", "Haskell"]
pruebaLuhn = [3,3,7,9,5,1,3,5,6,1,1,0,8,7,9,5]

myMod :: Int -> Int -> Int --Auxiliar para par e inpar. Implementa mod nativo de Haskell.
myMod n m = if (n < m) then n else myMod (n-m) m

-- Parte #1: ¿Por qué? porque sí.

potencia :: Int -> Int -> Int
potencia b 0 = 1
potencia 0 p = 0
potencia b 1 = b
potencia b p = if (b>1) --Evitar negativos
    then b * (potencia b (p-1)) --Potencia de cualq. número a calquier exponente
    else error "Entrada inválida." -- Si ingresan un negativo

suma_pares :: Int -> Int
suma_pares 0 = 0
suma_pares n = if (n>0) --Suma de pares positivos
    then (if (myMod n 2 == 0)
        then n + (suma_pares (n-1)) --Resul. suma de pares positivos
        else suma_pares (n-1))
    else 0 --Evitar negativos

triangular :: Int -> Int
triangular 0 = 0
    --El valor + el valor anterior
triangular n = if (n>0) then n + (triangular (n-1)) else 0

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = if (n>0) then (fibo (n-1) + fibo (n-2)) else 0

ultimo :: [a] -> a
ultimo []        = error "Lista sin elementos"
ultimo [a]       = a
ultimo (cbz:col) = ultimo col

reversa :: [a] -> [a]
reversa []        = []
reversa [a]       = [a]
reversa (cbz:col) = reversa col ++ [cbz]

-- Eq, a por lo que entendimos, hace que todas mis variables de tipo, puedan ser evaluadas con un == o un /=.
pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (cbz:col) = if a == cbz
    then True
    else pertenece a col --Si no existe a, llegará hasta [] donde es False


-- Parte #2: Pensar correcto es lo que hago...

data Categoria = Perfecto | Deficiente | Abundante

equival :: Categoria -> String --Equivalencia de data entonces imprime usa en la firma un Input/Output
equival Perfecto   = "Perfecto"
equival Abundante  = "Abundante"
equival Deficiente = "Deficiente"

nicomano :: Int -> String
nicomano 0 = error "0 es perfecto por def."
nicomano n = if (n>0)
    then equiv n (salicuotafac n (n-1)) --Acepta solo positivos
    else error "La entrada no es un número entero positivo."

salicuotafac :: Int -> Int -> Int
salicuotafac n 1 = 1 -- k es (n-1) pues para que aunque mod n n == 0, el mismo numero no es factor propio.
salicuotafac n k = if (myMod n k == 0) --Aqui sucede la clasificación de si son factores propios
    then k + salicuotafac n (k-1) -- Si lo es se suma
    else salicuotafac n (k-1) -- Si no pasa a la siguiente

equiv :: Int -> Int -> String
equiv n k = if (n == k) --Aqui la clasificación del número
    then equival Perfecto
    else (if (n > k)
            then equival Deficiente
            else equival Abundante
        )

{-
Nota: Podemos consider una lista como [cabeza:elemento:rabo], esto es, una cabeza, un elemento seguido a esta cabeza
y el resto de la lista. Esto nos ayuda a entender más fácil el funcionamiento del algoritmo de Luhn.
-}

-- Luhn

luhn :: [Int] -> Bool
luhn [] = False
luhn (x:y:xs) = if ((luhndigval (x:y:xs) == True) && (modTenLunh (sumaListLunh (multSecnLuhn (x:y:xs)))) == True) then True else False

-- Regla #0: Debemos considerar cada dígito como un número independiente.
luhndigval :: [Int] -> Bool
luhndigval [] = False --No hay elementos, no es válido
luhndigval [n] = if (n <= 9 && 0 <= n) then True else False
luhndigval (x:xs) = if (x <= 9 && x >= 0) -- el elemento en la cabeza, entero en la lista entre 0 y 9
    then True && (luhndigval xs) -- Si todos son validos True && True ... && True == True
    else error "Por favor ingrese elementos en la lista entre 0 y 9. Ej. [1,3,5,6,4]"  -- Si fuera [19,20] sin esta función sería válido, pero para que esto funcione bien tendría que ser [1,9,2,0].

{-
 Regla #1: Multiplicar por dos un número sí y otro no comenzando de izquierda a derecha.
 Para el tercer caso una lista con al menos dos elementos. Cada dígito impar lo multiplica por 2
-}

multSecnLuhn :: [Int] -> [Int]
multSecnLuhn []            = []
multSecnLuhn [n]           = [2*n]
-- Restamos -9 de una vez y asi la multiplicación no nos da elementos de dos cifras.
multSecnLuhn (cbz:ele:col) = restNineLunh ([2*cbz] ++ [ele] ++ multSecnLuhn col)

-- Regla #2: Restar 9 a cada número mayor a 9 para dejar números de una sola cifra.
restNineLunh :: [Int] -> [Int]
restNineLunh [] = []
restNineLunh [n] = if (n > 10) then [n-9] else [n]
restNineLunh (cbz:col) = if (cbz > 9)
    then ((cbz-9):restNineLunh col)
    else (cbz:(restNineLunh col))

-- Regla #3: Sumar todos los números.
sumaListLunh :: [Int] -> Int
sumaListLunh [n]       = n
sumaListLunh (cbz:col) = cbz + sumaListLunh col

-- Regla #4: Si el resultado es divisible por 10, es un número válido.
modTenLunh :: Int -> Bool
modTenLunh n = if (myMod n 10 == 0) then True else False

{-
divE :: Int -> Int -> Int
divE n m = divEAux n m 0

divEAux :: Int -> Int -> Int -> Int
divEAux n m acc = if n < m
        then acc -- Devuelve el acumulador, pues es el que contea cuantas veces se hixo la operación.
        else divEAux (n-m) m acc+1
-}

-- Collatz-

-- Función Auxiliar para pasosCollatz
longit :: [Int] -> Int
longit []        = 0
longit (cbz:col) = 1 + longit col

pasosCollatz :: Int -> Int
pasosCollatz 0 = 0
pasosCollatz 1 = 1
pasosCollatz n = if (n > 1)
    then (longit (listaCollatz n)) - 1 -- Longitud -1. Para no contar el paso 0 :).
    else error "Entrada inválida. Se necesita un número natural"

listaCollatz :: Int -> [Int]
listaCollatz 0 = [] --Sin elementos
listaCollatz 1 = [1] -- Será 1
listaCollatz n = if (myMod n 2 == 0) -- Es par?
   then [n] ++ listaCollatz (div n 2)
   else [n] ++ listaCollatz ((3*n) +1)
    --then [n] ++ listaCollatz (divE n 2) --Si, divide entre 2
    --else [n] ++ listaCollatz ((3*n)+1) -- No, multiplica por 3 y suma 1


-- Parte #3: Expresiones aritméticas.

data EA = N Int | Positivo EA | Negativo EA | Suma EA EA |
          Resta EA EA | Mult EA EA | Div EA EA | Mod EA EA |
          Pot EA EA

instance Show EA where
    show (N a)           = show a
    show (Positivo (a))  = show a
    show (Negativo (a))  = "(" ++ show a ++ ")"
    show (Suma (a) (b))  = show a ++ " + " ++ show b
    show (Resta (a) (b)) = show a ++ " - " ++ show b
    show (Mult (a) (b))  = show a ++ " * " ++ show b
    show (Div (a) (b))   = show a ++ " / " ++ show b
    show (Mod (a) (b))   = show a ++ " % " ++ show b
    show (Pot (a) (b))   = show a ++ " ^ " ++ show b

creaSumaEA :: Int -> Int -> EA
creaSumaEA a b =
    if (a<0 ) then Suma (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Suma (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Suma (Negativo (N a)) (Negativo (N a))
    else Suma (Positivo (N a)) (Positivo (N b))

creaRestaEA :: Int -> Int -> EA
creaRestaEA a b =
    if (a<0 ) then Resta (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Resta (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Resta (Negativo (N a)) (Negativo (N a))
    else Resta (Positivo (N a)) (Positivo (N b))

creaMultEA :: Int -> Int -> EA
creaMultEA a b =
    if (a<0 ) then Mult (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Mult (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Mult (Negativo (N a)) (Negativo (N a))
    else Mult (Positivo (N a)) (Positivo (N b))

creaDivEA :: Int -> Int -> EA
creaDivEA a b =
    if (a<0 ) then Div (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Div (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Div (Negativo (N a)) (Negativo (N a))
    else Div (Positivo (N a)) (Positivo (N b))

creaPotEA :: Int -> Int -> EA
creaPotEA a b =
    if (a<0 ) then Pot (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Pot (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Pot (Negativo (N a)) (Negativo (N a))
    else Pot (Positivo (N a)) (Positivo (N b))

creaModEA :: Int -> Int -> EA
creaModEA a b =
    if (a<0 ) then Mod (Negativo (N a)) (Positivo (N b))
    else if (b<0) then Mod (Positivo (N a)) (Negativo (N b))
    else if (a<0 && b<0) then Mod (Negativo (N a)) (Negativo (N a))
    else Mod (Positivo (N a)) (Positivo (N b))

--menorQue :: EA -> EA -> Bool
--menorQue a b =

--mayorQue :: EA -> EA -> Bool
--mayorQue a b =
