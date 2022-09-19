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

--myDiv :: Int -> Int -> Int
--myDiv n m = (myDiv n m)*m + (myMod n m)

-- Parte #1: ¿Por qué? porque sí.

potencia :: Int -> Int -> Int 
potencia b 0 = 1 --Potencia de cualq. número al exponente 0 es = 1.
potencia 0 p = 0 --Potencia de 0 a cualq. exponente es 0.
potencia b 1 = b --Potencia de cualq. número a la 1 es el mismo número.
potencia b p = if (b>1) --Evitar negativos
    then b * (potencia b (p-1)) --Potencia de cualq. número a calquier exponente
    else error "Entrada inválida." -- Si ingresan un negativo

suma_pares :: Int -> Int 
suma_pares 0 = 0 -- No hay q sumar
suma_pares n = if (n>0) --Suma de pares positivos
    then (if (mod n 2 == 0)
        then n + (suma_pares (n-1)) --Resul. suma de pares positivos
        else suma_pares (n-1)) --Resul s
    else 0 --Evitar negativos

triangular :: Int -> Int 
triangular 0 = 0
triangular n = if (n>0) then n + (triangular (n-1)) else 0
    --El valor + el valor anterior

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
reversa (cbz:col) = reversa col ++ [cbz] --Concatena listas

-- Eq a por lo que entendi, hace que todas mis variables de tipo, puedan ser evaluadas con un == o un /=.
pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (cbz:col) = if a == cbz
    then True
    else pertenece a col --Si no existe a, llegará hasta [] donde es False


-- Parte #2: Pensar correcto es lo que hago...

data Categoria = Perfecto | Deficiente | Abundante

equival :: Categoria -> IO() --Equivalencia de data entonces imprime usa en la firma un Input/Output
equival Perfecto   = print "Perfecto"
equival Abundante  = print "Abundante"
equival Deficiente = print "Deficiente"

nicomano :: Int -> IO()
nicomano 0 = error "0 es perfecto por def."
nicomano n = if (n>0)
    then equiv n (salicuotafac n (n-1)) --Acepta solo positivos
    else error "La entrada no es un número entero positivo."

salicuotafac :: Int -> Int -> Int
salicuotafac n 1 = 1 -- k es (n-1) pues para que aunque mod n n == 0, el mismo numero no es factor propio.
salicuotafac n k = if (myMod n k == 0) --Aqui sucede la clasificación de si son factores propios
    then k + salicuotafac n (k-1) -- Si lo es se suma
    else salicuotafac n (k-1) -- Si no pasa a la siguiente

equiv :: Int -> Int -> IO()
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

-- Función Principal
luhn :: [Int] -> Bool
luhn [] = False 
--luhn (cbz:cab:col) = if ((luhndigval (cbz:col) == True) && (modTenLunh (sumaListLunh (multSecnLuhn (cbz:cab:col))) == True))
  --then True 
  --else False
luhn (x:y:xs) = if (modTenLunh (sumaListLunh (multSecnLuhn (x:y:xs)))) == True then True else False


{-
 Regla #1: Multiplicar por dos un número sí y otro no comenzando de izquierda a derecha. 
 Para el tercer caso una lista con al menos dos elementos. Cada dígito impar lo multiplica por 2
-}
multSecnLuhn :: [Int] -> [Int]
multSecnLuhn []            = []
multSecnLuhn [n]           = [2*n] 
-- Restamos -9 de una vez y asi la multiplicación no nos da elementos de dos cifras. 
multSecnLuhn (cbz:elem:col) = restNineLunh ([2*cbz] ++ [elem] ++ multSecnLuhn col) 

-- Regla #2: Restar 9 a cada número mayor a 9 para dejar números de una sola cifra.
restNineLunh :: [Int] -> [Int]
restNineLunh [] = []
restNineLunh [n] = if (n > 10) then [n-9] else [n]
restNineLunh (cbz:col) = if (cbz > 9) 
    then ((cbz-9):restNineLunh col) 
    else (cbz:(restNineLunh col)) 

{-
-- Regla #
luhndigval :: [Int] -> Bool
luhndigval [] = False --No hay elementos, no es válido
luhndigval [n] = if (n > 9 && n < 0) then False else True
luhndigval (cbz:col) = if (cbz > 9 && cbz < 0) -- el elemento en la cabeza, entero en la lista entre 0 y 9
    then False
    else True && (luhndigval col) -- Si todos son validos True && True ... && True == True
-}

-- Regla #3: Sumar todos los números.
sumaListLunh :: [Int] -> Int
sumaListLunh [n]       = n
sumaListLunh (cbz:col) = cbz + sumaListLunh col 

-- Regla #4: Si el resultado es divisible por 10, es un número válido.
modTenLunh :: Int -> Bool
modTenLunh n = if (myMod n 10 == 0) then True else False

-------------------------------------------------------------------


longit :: [Int] -> Int
longit []        = 0
longit (cbz:col) = 1 + longit col

{-
divE :: Int -> Int -> Int
divE n m = divEAux n m 0

divEAux :: Int -> Int -> Int -> Int
divEAux n m acc = if n < m
        then acc -- Devuelve el acumulador, pues es el que contea cuantas veces se hixo la operación.
        else divEAux (n-m) m acc+1
-}

pasosCollatz :: Int -> Int
pasosCollatz 0 = 0
pasosCollatz 1 = 1
pasosCollatz n = if (n > 1)
    then (longit (listaCollatz n)) - 1 -- Longitud -1. Para no contar el paso 0 :).
    else error "Entrada inválida. Se necesita un número natural"

-- Si podemos ocupar 'div' ya fregó por que justo resolvemos Collatz con dos métodos

listaCollatz :: Int -> [Int]
listaCollatz 0 = [] --Sin elementos
listaCollatz 1 = [1] -- Será 1
listaCollatz n = if (myMod n 2 == 0) -- Es par?
   then [n] ++ listaCollatz (div n 2)
   else [n] ++ listaCollatz ((3*n) +1)
    --then [n] ++ listaCollatz (divE n 2) --Si, divide entre 2
    --else [n] ++ listaCollatz ((3*n)+1) -- No, multiplica por 3 y suma 1


-- Parte #3: Expresiones aritméticas.
{-
data EA = N Int | Positivo EA | Negativo EA | Suma EA EA | Resta EA EA | Mult EA EA | Div EA EA | Mod EA EA | Pot EA EA

creaSumaEA :: Int -> Int -> EA
creaRestaEA :: Int -> Int -> EA
creaMultEA :: Int -> Int -> EA
creaDivEA :: Int -> Int -> EA
creaModEA :: Int -> Int -> EA
creaPotEA :: Int -> Int -> EA
-}
