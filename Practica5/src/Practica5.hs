{-
Estructuras Discretas 2023-1
Alumno: Robledo Ramírez Isaac
Número de cuenta: 320140655
Oyente: Paredes Zamudio Luis Daniel
Número de Cuenta: 318159926
-}

--Alias para variables
type Nombre = String

--Tipo para las proposiciones
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | Syss LProp LProp | Neg LProp deriving (Eq)

data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux | Beta [LProp] Tableaux Tableaux

instance Show LProp where
    show (VarP a)    = a
    show F          = "False"
    show T          = "True"
    show (Neg a)    = "¬" ++ show a
    show (Conj a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Disy a b) = "(" ++ show a ++ " v " ++ show b ++ ")"
    show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Syss a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

-- 1. literales Función que nos dice si en una lista de fórmulas, todas son literales.
literales:: [LProp] -> Bool
literales l = and $ map esLiteral l
-- literales = and . map esliteral

-- Función auxiliar que hace el juicio de si algo es una literal o no.
esLiteral :: LProp -> Bool
esLiteral (VarP a) = True
esLiteral (Neg a) = esLiteral a
esLiteral T = True
esLiteral F = True
esLiteral _ = False

-- 2. nextF Función que regresa la primera fórmula que no es literal de una lista de fórmulas.
nextF:: [LProp] -> LProp
nextF (a:xs) = if (esLiteral a == False) then a else nextF xs

-- 3. alpha Nos dice si una fórmula f es una fórmula α
alpha :: LProp -> Bool
alpha (Conj a b) = True
alpha (Neg (Disy a b)) = True
alpha a = False

-- 4. beta  Nos dice si una fórmula f es una fórmula β
beta :: LProp -> Bool
beta (Disy a b) = True
beta (Neg (Conj a b)) = True
beta (Impl a b) = True
beta a = False

-- 5. sigma Nos dice si una fórmula f es una fórmula σ
sigma:: LProp -> Bool 
sigma (Neg T)       = False
sigma (Neg F)       = True
sigma (Neg (Neg a)) = True
sigma a             = False

-- 6. expAlpha Dada una lista de fórmulas l y una fórmula f realiza la expansión alpha de f dentro la lista l.
expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha l f@(Conj a b) = a:b:ln where ln = removeItem f l  
expAlpha l x@(Neg (Disy a b)) =  a:b:ln where ln = removeItem x l

--Auxiliar
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


-- 7. expBeta  Dada una lista de fórmulas l y una fórmula f realiza la expansión beta de f sobre la lista l.
expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta l q@(Disy x y) = 

{- expBeta (a:xs) w@(Neg (Conj x y)) = a:b:ln where ln = removeItem w l
expBeta (a:xs) u@(Impl x y) = a:b:ln where ln = removeItem u l    
 -}

-- 8. expSigma Dada una lista de fórmulas l y una fórmula f , realiza la expansión sigma de f sobre la lista l.
{- expSigma:: [LProp] -> LProp -> ([LProp], [LProp])
expSigma l f =  -}

-- 9. consTableaux Construye el tableau a partir de una fórmula.