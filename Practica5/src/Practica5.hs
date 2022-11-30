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

data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux Tableaux | Beta [LProp] Tableaux Tableaux deriving (Eq)

instance Show LProp where
    show (VarP a)   = a
    show F          = "False"
    show T          = "True"
    show (Neg a)    = "¬" ++ show a
    show (Conj a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
    show (Disy a b) = "(" ++ show a ++ " v " ++ show b ++ ")"
    show (Impl a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Syss a b) = "(" ++ show a ++ " <-> " ++ show b ++ ")"

instance Show Tableaux where
    show (Hoja a) = "\n\t(Hoja " ++ show a ++ ")"
    show (Alpha a b c) = "\nAlpha " ++ show a ++ "(" ++ show b ++ ")(" ++ show c ++ ")"
    show (Beta a b c)  = "\nBeta " ++ show a ++ "(" ++ show b ++ ")(" ++ show c ++ ")"

-- 1. literales Función que nos dice si en una lista de fórmulas, todas son literales.
literales:: [LProp] -> Bool
literales l = and $ map esLiteral l
-- literales = and . map esliteral

-- Función auxiliar que hace el juicio de si algo es una literal o no.
esLiteral :: LProp -> Bool
esLiteral (VarP a) = True
esLiteral (Neg a)  = esLiteral a
esLiteral T        = True
esLiteral F        = True
esLiteral _        = False

-- 2. nextF Función que regresa la primera fórmula que no es literal de una lista de fórmulas.
nextF:: [LProp] -> LProp
nextF (a:xs) = if (esLiteral a == False) then a else nextF xs

-- 3. alpha Nos dice si una fórmula f es una fórmula α
alpha :: LProp -> Bool
alpha (Conj a b)       = True
alpha (Neg (Disy a b)) = True
alpha (Neg (Impl a b)) = True
alpha a                = False

-- 4. beta  Nos dice si una fórmula f es una fórmula β
beta :: LProp -> Bool
beta (Disy a b)       = True
beta (Neg (Conj a b)) = True
beta (Impl a b)       = True
beta a                = False

-- 5. sigma Nos dice si una fórmula f es una fórmula σ
sigma:: LProp -> Bool
sigma (Neg T)       = False
sigma (Neg F)       = True
sigma (Neg (Neg a)) = True
sigma a             = False

-- 6. expAlpha Dada una lista de fórmulas l y una fórmula f realiza la expansión alpha de f dentro la lista l.
expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha l f@(Conj a b) = if (elem f l) then a:b:ln else error "El término dado, no es elemento de la lista." where ln = removeItem f l
expAlpha l x@(Neg (Disy a b)) = if (elem x l) then (Neg a):(Neg b):ln else error "El término dado, no es elemento de la lista."
                                where ln = removeItem x l
expAlpha l z@(Neg (Impl a b)) = if (elem z l) then (Neg a):b:ln else error "El término dado, no es elemento de la lista." where ln = removeItem z l
expAlpha l a = if (elem a l) then error "Tipo de LProp no válido para expAlpha."
            else error "En la lista no se encuentra el término dado y/o el termino dado para la expansión no es válido para expAlpha."

--Auxiliar para remover elementos de las listas de formulas. 
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- 7. expBeta  Dada una lista de fórmulas l y una fórmula f realiza la expansión beta de f sobre la lista l.
expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta l q@(Disy x y)       = if (elem q l) then (x:ln, (y:ln))
                               else error "El término dado, no es elemento de la lista." where ln = removeItem q l
expBeta l w@(Neg (Conj x y)) = if (elem w l) then ((Neg x):ln, ((Neg y):ln))
                               else error "El término dado, no es elemento de la lista." where ln = removeItem w l
expBeta l j@(Impl x y)       = if (elem j l) then ((Neg x):ln, (y:ln))
                               else error "El término dado, no es elemento de la lista." where ln = removeItem j l
expBeta l a                  = if (elem a l) then error "Tipo de LProp no válido para expBeta."
                               else error "En la lista no se encuentra el término dado y/o el termino dado para la expansión no es válido para expBeta."

-- 8. expSigma Dada una lista de fórmulas l y una fórmula f , realiza la expansión sigma de f sobre la lista l.
expSigma:: [LProp] -> LProp -> ([LProp])
expSigma l u@(Neg(Neg(x))) = if (elem u l) then (x:ln) else error "El término dado, no es elemento de la lista."  
                            where ln = removeItem u l
expSigma l a               = if (elem a l) then error "Tipo de LProp no válido para expSigma."
                            else error "En la lista no se encuentra el término dado y/o el termino dado para la expansión no es válido para expSigma."

-- 9. consTableaux Construye el tableau a partir de una fórmula.
consTableaux :: LProp -> Tableaux
consTableaux (VarP a) = Hoja [(VarP a)]
consTableaux (Conj a b) = Alpha [(Conj a b)] (consTableaux a) (consTableaux b)
consTableaux (Neg(Disy a b)) = Alpha [(Neg(Disy a b))] (consTableaux (Neg a)) (consTableaux (Neg b))
consTableaux (Neg(Impl a b)) = Alpha [(Neg(Impl a b))] (consTableaux a) (consTableaux (Neg b))
consTableaux (Disy a b) = Beta [(Disy a b)] (consTableaux a) (consTableaux b)
consTableaux (Neg(Conj a b)) = Beta [(Neg(Conj a b))] (consTableaux (Neg a)) (consTableaux (Neg b))
consTableaux (Impl a b) = Beta [(Impl a b)] (consTableaux (Neg a)) (consTableaux b)
consTableaux (Neg(Neg a)) = consTableaux a
consTableaux (Neg(VarP a)) = Hoja [(Neg(VarP a))]
