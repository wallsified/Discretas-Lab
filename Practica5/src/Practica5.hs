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
esLiteral _ = Falsecd

-- 2. nextF Función que regresa la primera fórmula que no es literal de una lista de fórmulas.
nextF:: [LProp] -> LProp
nextF _ = error "implementar"

-- 3. alpha Nos dice si una fórmula f es una fórmula α
alpha :: LProp -> Bool
alpha _ = error "implementar"
-- si nextF me da la siguiente formula no literal, la quitamos (funcion auxiliar?) 

-- 4. beta  Nos dice si una fórmula f es una fórmula β
beta :: LProp -> Bool
beta _ = error "implementar"

-- 5. sigma Nos dice si una fórmula f es una fórmula σ
sigma:: LProp -> Bool 
sigma _ = error "implementar"

{- sigma (Neg T)       = F
sigma (Neg F)       = T
sigma (Neg (Neg a)) = T
sigma a             = F
 -}

-- 6. expAlpha Dada una lista de fórmulas l y una fórmula f realiza la expansión alpha de f dentro la lista l.
--expAlpha :: [LProp] -> LProp -> [LProp]
-- expAlpha l f@(Conj a b) = let ln = quita f l in a:b:ln
-- expAlpha l (Conj a b) = a:b:ln where ln = quita f l   


-- 7. expBeta  Dada una lista de fórmulas l y una fórmula f realiza la expansión beta de f sobre la lista l.
-- expBeta :: [LProp] -> LProp -> ([LProp], [LProp])

-- 8. expSigma Dada una lista de fórmulas l y una fórmula f , realiza la expansión sigma de f sobre la lista l.

-- 9. consTableaux Construye el tableau a partir de una fórmula.
