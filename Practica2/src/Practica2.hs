{-
Estructuras Discretas 2023-1
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
Alumno:
No. de Cuenta:
-}
-- Cosa innecesaria. 

-- Parte #1: ¿Por qué? porque sí.

potencia :: Int -> Int

sumaPares :: Int -> Int

triangular :: Int -> Int

fibo :: Int -> Int

ultimo :: [a] -> a

reversa :: [a] -> [a]

pertenece :: [a] -> a -> Bool

-- Parte #2: Pensar correcto es lo que hago...


-- Parte #3: Expresiones aritméticas.

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

