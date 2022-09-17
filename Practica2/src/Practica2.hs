{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

-- Parte #1: ¿Por qué? porque sí.

potencia :: Int -> Int -> Int --Firma
potencia b 0 = 1 --Potencia de cualq. número al exponente 0 es = 1.
potencia 0 p = 0 --Potencia de 0 a cualq. exponente es 0.
potencia b 1 = b --Potencia de cualq. número a la 1 es el mismo número.
potencia b p = if (b>1) --Evitar negativos
then b * (potencia b (p-1)) --Potencia de cualq. número a calquier exponente 
else error "Entrada inválida." -- Si ingresan un negativo

suma_pares :: Int -> Int --Firma
suma_pares 0 = 0 -- No hay q sumar
suma_pares n = if (n>0) --Suma de pares positivos
then (if (mod n 2 == 0)
then n + (suma_pares (n-1)) --Resul. suma de pares positivos
else suma_pares (n-1)) --Resul s
else error "Entrada inválida." --Evitar negativos


triangular :: Int -> Int --Firma
triangular 0 = 0 
triangular n = if (n>0)
then n + (triangular (n-1)) --El valor + el valor anterior
else error "Entrada inválida."

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = if (n>0)
then fibo (n-1) + fibo (n-2) --Hasta que n = 2
else error "Entrada inválida."

ultimo :: [a] -> a
ultimo [] = error "Lista sin elementos"
ultimo [a] = a -- El ultimo de una lista de 1 elemento es ese mismo
ultimo (cbz:col) = ultimo col --No hará nada hasta que tenga un valor, siendo [a]

reversa :: [a] -> [a]
reversa [] = []
reversa [a] = [a]
reversa (cbz:col) = (reversa (col)) ++ [cbz] --Concatena listas
--Con los char 'x' si le hace la reversa pero hace esto:
-- reversa ['a','b','c'] = "cba"
-- Y no, ['c','b','a'] 


-- Eq a por lo que entendi, hace que todas mis variables de tipo, puedan ser evaluadas con un == o un /=.
pertenece :: Eq a => a -> [a] -> Bool 
pertenece a [] = False
pertenece a (cbz:col) = if a == cbz
then True
else pertenece a col --Si no existe a, llegará hasta [] donde es False


-- Parte #2: Pensar correcto es lo que hago...
--NICOMANO
data Categoria = Perfecto | Deficiente | Abundante --Aquí la data

equival :: Categoria -> IO() --Equivalencia de data entonces imprime usa en la firma un Input/Output 
equival Perfecto = print "Perfecto" 
equival Abundante = print "Abundante"
equival Deficiente = print "Deficiente"

nicomano :: Int -> IO()
nicomano 0 = error "0 es perfecto por def."
nicomano n = if (n>0)
then equiv n (salicuotafac n (n-1)) --Acepta solo positivos
else error "La entrada no es un número entero positivo."

salicuotafac :: Int -> Int -> Int 
salicuotafac n 1 = 1 -- k es (n-1) pues para que aunque mod n n == 0, el mismo numero no es factor propio.
salicuotafac n k = if (mod n k == 0) --Aqui sucede la clasificación de si son factores propios
then k + salicuotafac n (k-1) -- Si lo es se suma
else salicuotafac n (k-1) -- Si no pasa a la siguiente

equiv :: Int -> Int -> IO() --La misma salida que equival para el match
equiv n k = if (n == k) --Aqui la clasificación del número
then equival Perfecto --perfecto
else (if (n > k) 
then equival Deficiente --deficiente
else equival Abundante)--abundante Falta imprimir


--LUHN
--F PRINCIPAL
-- (cbz:cab:col) es para mi una cabeza, un elemento detras de la cabeza y una cola.
-- es decir, [cabeza, elemento detras de cabeza, cola........]
luhn :: [Int] -> Bool
luhn [] = False --No es 
luhn (cbz:cab:col) = if ((luhndigval (cbz:col) == True) && (boolluhn (luhnsuma (luhnseg (cbz:cab:col))) == True))
then True --La validacion de que cada elemento de la lista es un solo digito, y si la suma de todo el proceso es es divisible entre 10.
else False --Alguna o las dos arroja false o error.

--F AUX
luhndigval :: [Int] -> Bool
luhndigval [] = False --No hay elementos, no es válido
luhndigval [n] = if (n > 9 && n < 0) -- solo enteros entre 0 y 9
then False
else True
luhndigval (cbz:col) = if (cbz > 9 && cbz < 0) -- el elemento en la cabeza, entero en la lista entre 0 y 9
then False
else True && (luhndigval col) -- Si todos son validos True && True ... && True == True

--F AUX
luhnseg :: [Int] -> [Int]
luhnseg [] = []
luhnseg [n] = [2*n] -- Un extra sería dígito impar 
luhnseg (cbz:cab:col) = luhndiez ([2*cbz] ++ [cab] ++ luhnseg col) --Una lista con al menos dos elementos. Cada dígito impar lo multiplica por 2
-- Si se quiere en dígitos pares [n] = [n] y luhnseg (cbz:cab:col) = luhndiez ([cbz] ++ [2*cab] ++ luhnseg col)

--F AUX
luhndiez :: [Int] -> [Int]
luhndiez [] = []
luhndiez [n] = if (n >= 10) --Elemento mayor a 9
then [n-9] --Resta los 9
else [n]
luhndiez (cbz:col) = if (cbz > 9) --Elemento en la cabeza, mayor a 9
then ((cbz-9):luhndiez col) --Resta 9 y une a la lista
else (cbz:(luhndiez col)) --No le hagas nada a la cabeza y continua con la cola

--F AUX
luhnsuma :: [Int] -> Int
luhnsuma [n] = n
luhnsuma (cbz:col) = cbz + luhnsuma col --suma los elementos

--F AUX
boolluhn :: Int -> Bool
boolluhn n = if (mod n 10 == 0) --El entero es divisble entre 10.
then True
else False

--COLLATZ
--F DE LAB 1
--F AUX
longit :: [Int] -> Int
longit [] = 0
longit (cbz:col) = 1 + longit col
--F AUX
divE :: Int -> Int -> Int
divE n m = divEAux n m 0 
-- Aquí se llama a la f. aux, la cual pide 3 valores, siendo uno el acumulador, que empieza en 0.
--F AUX DE LA F AUX
-- FUNCIÓN AUXILIAR
divEAux :: Int -> Int -> Int -> Int
divEAux n m acc = if n < m 
        then acc -- Devuelve el acumulador, pues es el que contea cuantas veces se hixo la operación. 
        else divEAux (n-m) m acc+1

pasosCollatz :: Int -> Int
pasosCollatz 0 = 0 
pasosCollatz 1 = 1 
pasosCollatz n = if (n > 1)
then (longit (listaCollatz n)) - 1 -- Longitud -1. Para no contar el paso 0 :). 
else error "Entrada inválida. Se necesita un número natural"

listaCollatz :: Int -> [Int]
listaCollatz 0 = [] --Sin elementos
listaCollatz 1 = [1] -- Será 1
listaCollatz n = if (mod n 2 == 0) -- Es par?
then [n] ++ listaCollatz (divE n 2) --Si, divide entre 2
else [n] ++ listaCollatz ((3*n)+1) -- No, multiplica por 3 y suma 1
-- Parte #3: Expresiones aritméticas.
{-
data EA = N Int | Positivo EA | Negativo EA | Suma EA EA | Resta EA EA | Mult EA EA | Div EA EA | Mod EA EA | Pot EA EA

creaSumaEA :: Int -> Int -> EA
creaRestaEA :: Int -> Int -> EA
creaMultEA :: Int -> Int -> EA
creaDivEA :: Int -> Int -> EA
creaModEA :: Int -> Int -> EA
creaPotEA :: Int -> Int -> EA

-- Something moar and then
menorque :: EA -> EA -> Bool
mayorque :: EA -> EA -> Bool
-}
