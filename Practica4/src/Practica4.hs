{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

{-
PA COPIAR
(Var a) =
PFalse =
PTrue =
(Neg a) =
(Conj a b) =
(Disy a b) =
(Impl a b) =
(Syss a b) =
-}

data LProp = PTrue | PFalse | Var Nombre | Neg LProp | Conj LProp LProp |
             Disy LProp LProp | Impl LProp LProp | Syss LProp LProp

type Nombre = String
type Asignacion = [(Nombre, Int)]

instance Show LProp where
    show (Var a)    = show a
    show PFalse     = show False
    show PTrue      = show True

    show (Neg a)    = "!" ++ show a
    show (Conj a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Disy a b) = "(" ++ show a ++ " v " ++ show b ++ ")"
    show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Syss a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

-- 1. vars Función que recibe una LProp y regresa la lista con todas las variables que aparecen en la expresión.
-- Hicimos todos con firma de Strings por la ordenación, gracias a la comparativa.

vars :: LProp -> [String]
vars (Var a)    = [a] -- También se poddría poner [(Var a] y gracias a la instancia de Show de igual manera sería una lista de Strings, pero, según nosotros ahorramos tiempo solo pasando, como vimos en clase :).
vars PFalse     = []
vars PTrue      = []
vars (Neg a)    = vars a
vars (Conj a b) = varsAux2(varsAux1(vars a ++ vars b))
vars (Disy a b) = varsAux2(varsAux1(vars a ++ vars b))
vars (Impl a b) = varsAux2(varsAux1(vars a ++ vars b))
vars (Syss a b) = varsAux2(varsAux1(vars a ++ vars b))

-- Función auxiliar que ordena los elementos
varsAux1 :: [String] -> [String]
varsAux1 [] = []
varsAux1 [a] = [a]
varsAux1 (x:xs) = if x == (takeS 1 xs) then varsAux1 xs else
                 (if x < (takeS 1 xs) then [x] ++ varsAux1 xs else varsAux1 xs ++ [x])

-- Funcion que elimina duplicados
varsAux2 :: [String] -> [String]
varsAux2 []     = []
varsAux2 [a]    = [a]
varsAux2 (x:xs) = if x == takeS 1 xs then varsAux2 xs else [x] ++ varsAux2 xs

--Función que obtiene un elemento de una lista
takeS :: Int -> [String] -> String
takeS n []       = []
takeS 0 (x:xs)   = []
takeS 1 (x:xs)   = x
takeS n (x : xs) = takeS (n-1) xs


-- 2. asocia_der Función que recibe una LProp y aplica la ley de la asociatividad hacia la derecha sobre los elementos de la expresión.
asocia_der :: LProp -> LProp
asocia_der (Var a)             = (Var a)
asocia_der PFalse              = PFalse
asocia_der PTrue               = PTrue

asocia_der (Conj (Conj a b) c) = (Conj a (asocia_der (Conj b c)))
asocia_der (Disy (Disy a b) c) = (Disy a (asocia_der (Disy b c)))
asocia_der (Impl (Impl a b) c) = (Impl a (asocia_der (Impl b c)))
asocia_der (Syss (Syss a b) c) = (Syss a (asocia_der (Syss b c)))

asocia_der (Neg a)             = (Neg a)
asocia_der (Conj a b)          = (Conj a b)
asocia_der (Disy a b)          = (Disy a b)
asocia_der (Impl a b)          = (Impl a b)
asocia_der (Syss a b)          = (Syss a b)

-- 3. asocia_der Función que recibe una LProp y aplica la ley de la asociatividad hacia la izquierda sobre los elementos de la expresión.
asocia_izq :: LProp -> LProp
asocia_izq (Var a)             = (Var a)
asocia_izq PFalse              = PFalse
asocia_izq PTrue               = PTrue

asocia_izq (Conj a (Conj b c)) = (Conj (asocia_izq (Conj a b)) c)
asocia_izq (Disy a (Disy b c)) = (Disy (asocia_izq (Disy a b)) c)
asocia_izq (Impl a (Impl b c)) = (Impl (asocia_izq (Impl a b)) c)
asocia_izq (Syss a (Syss b c)) = (Syss (asocia_izq (Syss a b)) c)

asocia_izq (Neg a)             = (Neg a)
asocia_izq (Conj a b)          = (Conj a b)
asocia_izq (Disy a b)          = (Disy a b)
asocia_izq (Impl a b)          = (Impl a b)
asocia_izq (Syss a b)          = (Syss a b)

-- 4. conm Función que recibe una LPropr y aplica la ley de la conmutatividad de forma exhaustiva sobre los elementos de la expresión cuyo operador lógico sea conjunción o disyunción.
conm :: LProp -> LProp
conm (Var a)    = (Var a)
conm PFalse     = PFalse
conm PTrue      = PTrue
conm (Neg a)    = (Neg (conm a))
conm (Conj a b) = (Conj (conm b) (conm a))
conm (Disy a b) = (Disy (conm b) (conm a))
conm (Impl a b) = (Impl (conm a) (conm b))
conm (Syss a b) = (Syss (conm a) (conm b))

-- 8. dobleNeg Función que quita las dobles negaciones de una LProp.
dobleNeg :: LProp -> LProp
dobleNeg (Var a)       = (Var a)
dobleNeg PFalse        = PFalse
dobleNeg PTrue         = PTrue
dobleNeg (Neg (Neg a)) = dobleNeg a
dobleNeg (Neg a)       = (Neg (dobleNeg a))
dobleNeg (Conj a b)    = (Conj (dobleNeg a) (dobleNeg b))
dobleNeg (Disy a b)    = (Disy (dobleNeg a) (dobleNeg b))
dobleNeg (Impl a b)    = (Impl (dobleNeg a) (dobleNeg b))
dobleNeg (Syss a b)    = (Syss (dobleNeg a) (dobleNeg b))

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
