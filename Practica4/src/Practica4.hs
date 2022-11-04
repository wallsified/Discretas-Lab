{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp |
             Disy LProp LProp | Impl LProp LProp | Syss LProp LProp

type Nombre = String
type Asignacion = [(Nombre, Int)]

instance Show LProp where
    show (Var a)    = show a
    show PFalse     = show False
    show PTrue      = show True
    show (Neg a)    = "¬" ++ show a
    show (Conj a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Disy a b) = "(" ++ show a ++ " v " ++ show b ++ ")"
    show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Syss a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

-- 1. vars Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.

vars :: LProp -> [Nombre]
vars (Var a)    = [a] -- También se podría poner [(Var a] y gracias a la instancia de Show de igual manera sería una lista de Strings, pero, según nosotros ahorramos tiempo solo pasando, como vimos en clase :).
vars PFalse     = []
vars PTrue      = []
vars (Neg a)    = vars a
vars (Conj a b) = noDouble(ordena(vars a ++ vars b))
vars (Disy a b) = noDouble(ordena(vars a ++ vars b))
vars (Impl a b) = noDouble(ordena(vars a ++ vars b))
vars (Syss a b) = noDouble(ordena(vars a ++ vars b))

-- Función auxiliar que ordena los elementos
ordena :: [String] -> [String]
ordena [] = []
ordena [a] = [a]
ordena (x:xs) = if x == (takeS 1 xs) then ordena xs else
                 (if x < (takeS 1 xs) then [x] ++ ordena xs else ordena xs ++ [x])

-- Funcion que elimina duplicados
noDouble :: [String] -> [String]
noDouble []     = []
noDouble [a]    = [a]
noDouble (x:xs) = if (x == takeS 1 xs) then noDouble xs else [x] ++ noDouble xs

--Función que obtiene un elemento de una lista
takeS :: Int -> [String] -> String
takeS n []       = []
takeS 0 (x:xs)   = []
takeS 1 (x:xs)   = x
takeS n (x : xs) = takeS (n-1) xs

-- 2. asocia_der Función que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de la expresión.
asocia_der :: LProp -> LProp
asocia_der (Neg a)             = (Neg (asocia_der a))
asocia_der (Conj (Conj a b) c) = (Conj a (asocia_der (Conj b c)))
asocia_der (Disy (Disy a b) c) = (Disy a (asocia_der (Disy b c)))
asocia_der (Impl (Impl a b) c) = (Impl a (asocia_der (Impl b c)))
asocia_der (Syss (Syss a b) c) = (Syss a (asocia_der (Syss b c)))
asocia_der a                   = a

-- Nota. Si consideramos queque asociar, conmutar, etc, sobre uno mismo, a menos que esté especificado, da a si mismo, entonces solo debemos explicar los casos específicos y no uno por uno (aunque igual sería válido.)

-- 3. asocia_der Función que recibe una LProp y aplica la ley de la asociatividad hacia la izquierda sobre los elementos de la expresión.
asocia_izq :: LProp -> LProp
asocia_izq (Neg a)             = Neg (asocia_izq a)
asocia_izq (Conj a (Conj b c)) = (Conj (asocia_izq (Conj a b)) c)
asocia_izq (Disy a (Disy b c)) = (Disy (asocia_izq (Disy a b)) c)
asocia_izq (Impl a (Impl b c)) = (Impl (asocia_izq (Impl a b)) c)
asocia_izq (Syss a (Syss b c)) = (Syss (asocia_izq (Syss a b)) c)
asocia_izq a                   = a

-- 4. conm Función que recibe una LPropr y aplica la ley de la conmutatividad de forma exhaustiva sobre los elementos de la expresión cuyo operador lógico sea conjunción o disyunción.
conm :: LProp -> LProp
conm (Neg a)    = Neg (conm a)
conm (Conj a b) = (Conj (conm b) (conm a))
conm (Disy a b) = (Disy (conm b) (conm a))
conm (Impl a b) = (Impl (conm a) (conm b))
conm (Syss a b) = (Syss (conm a) (conm b))
conm a          = a

-- 5. dist Función que recibe una LProp y aplica la ley de distributividad de forma exhaustiva sobre toda la expresión.
dist :: LProp -> LProp
dist (Conj x (Disy y z)) = Disy (Conj x y) (Conj x z)
dist (Conj (Disy y z) x) = Disy (Conj y x) (Conj z x)
dist (Disy x (Conj y z)) = Conj (Disy x y) (Disy x z)
dist (Disy (Conj y z) x) = Conj (Disy y x) (Disy z x)
dist (Conj x y)          = Conj (dist y) (dist x)
dist (Disy x y)          = Disy (dist y) (dist x)
dist (Syss x y)          = Syss (dist x) (dist y)
dist (Impl x y)          = Impl (dist x) (dist y)
dist a                   = a

-- 6. deMorgan Función que le aplica a una LProp las leyes de De morgan.
deMorgan :: LProp -> LProp
deMorgan (Neg (Conj x y)) = Disy (Neg x) (Neg y)
deMorgan (Neg (Disy x y)) = Conj (Neg x) (Neg y)
deMorgan (Neg (Neg a))    = Neg (Neg (deMorgan a))
deMorgan (Neg a)          = Neg (deMorgan a)
deMorgan (Syss x y)       = Syss (deMorgan x) (deMorgan y)
deMorgan (Impl x y)       = Impl (deMorgan x) (deMorgan y)
deMorgan x                = x

-- 7. equiv_op Función que recibe una LProp y aplica la equivalencia de operadores que se describe al inicio de este documento
equiv_op :: LProp -> LProp
equiv_op (Neg a)    = Neg (equiv_op a)
equiv_op (Conj a b) = Conj (equiv_op a) (equiv_op b)
equiv_op (Disy a b) = Disy (equiv_op a) (equiv_op b)
equiv_op (Impl a b) = Disy (Neg (equiv_op a)) (equiv_op b)
equiv_op (Syss a b) = Conj (Disy (Neg (equiv_op b)) (equiv_op a)) (Disy (Neg (equiv_op a)) (equiv_op b)) -- En el pdf vienen con dos resultados diferentes o (b a a b) o (a b b a).
equiv_op a = a

-- 8. dobleNeg Función que quita las dobles negaciones de una LProp.
dobleNeg :: LProp -> LProp
dobleNeg (Neg (Neg a)) = dobleNeg a
dobleNeg (Neg a)       = (Neg (dobleNeg a))
dobleNeg (Conj a b)    = (Conj (dobleNeg a) (dobleNeg b))
dobleNeg (Disy a b)    = (Disy (dobleNeg a) (dobleNeg b))
dobleNeg (Impl a b)    = (Impl (dobleNeg a) (dobleNeg b))
dobleNeg (Syss a b)    = (Syss (dobleNeg a) (dobleNeg b))
dobleNeg a             = a

-- 9. Función que redibe una LProp y contesta con el número de conectivos lógicos en la expresión.
num_conectivos :: LProp -> Int
num_conectivos (Var a)    = 0
num_conectivos PTrue      = 0
num_conectivos PFalse     = 0
num_conectivos (Neg a)    = 1 + (num_conectivos a)
num_conectivos (Conj a b) = 1 + (num_conectivos a) + (num_conectivos b)
num_conectivos (Disy a b) = 1 + (num_conectivos a) + (num_conectivos b)
num_conectivos (Impl a b) = 1 + (num_conectivos a) + (num_conectivos b)
num_conectivos (Syss a b) = 1 + (num_conectivos a) + (num_conectivos b)

-- 10. interpretacion Esta función va a tomar una LProp ψ y una asignación para regresar la interpretacion de ψ a partir de los valores de la asignación.
interpretacion:: LProp -> Asignacion -> Int
interpretacion PTrue asig = 1
interpretacion PFalse asig = 0
interpretacion (Var a) asig = asignaValor a asig
interpretacion (Neg expr)  asig = (interpretacion expr asig)-1
interpretacion (Conj exp1 exp2) asig = if (interpretacion exp1 asig) == 1 && (interpretacion exp2 asig) == 1 then 1 else 0
interpretacion (Disy exp1 exp2) asig = if (interpretacion exp1 asig) == 0 || (interpretacion exp2 asig) == 0 then 1 else 0
interpretacion (Impl exp1 exp2) asig = if (interpretacion exp2 asig) == 1 || (interpretacion exp1 asig)-1 == 0 then 0 else 1
interpretacion (Syss exp1 exp2) asig = if (interpretacion exp1 asig) ==  (interpretacion exp2 asig) then 1 else 0

--Funcion que dada una asignación, coloca a cada letra el valor requerido. Auxiliar de interpretación
asignaValor:: Eq a => a -> [(a,b)] -> b
asignaValor x ((a,b):xs) = if a == x then b else asignaValor x xs
