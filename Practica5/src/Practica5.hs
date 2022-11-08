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

instance Show LProp where
    show (VarP a)    = show a
    show F     = show False
    show T      = show True
    show (Neg a)    = "¬" ++ show a
    show (Conj a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Disy a b) = "(" ++ show a ++ " v " ++ show b ++ ")"
    show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Syss a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"


{- --Tipo para el Tableaux
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux | Beta [LProp] Tableaux Tableaux deriving (Show, Eq)
 -}
{- instance Show Tableaux where
    show (Hoja [x] ) =
    show (Alpha [x] a) =
    show (Beta [x] a b) = -}

-- 1. literales Función que nos dice si en una lista de fórmulas, todas son literales.

-- 2. nextF Función que regresa la primera fórmula que no es literal de una lista de fórmulas.

-- 3. alpha Nos dice si una fórmula f es una fórmula α

-- 4. beta  Nos dice si una fórmula f es una fórmula β

-- 5. sigma Nos dice si una fórmula f es una fórmula σ

-- 6. expAlpha Dada una lista de fórmulas l y una fórmula f realiza la expansión alpha de f dentro la lista l.

-- 7. expBeta  Dada una lista de fórmulas l y una fórmula f realiza la expansión beta de f sobre la lista l.

-- 8. expSigma Dada una lista de fórmulas l y una fórmula f , realiza la expansión sigma de f sobre la lista l.

-- 9. consTableaux Construye el tableau a partir de una fórmula.
