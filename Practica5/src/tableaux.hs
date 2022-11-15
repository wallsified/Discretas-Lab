type Nombre = String
data Lprop = T | F 
        | VarP Nombre
        | Conj Lprop Lprop
        | Disy Lprop Lprop
        | Impl Lprop Lprop 
        | Syss Lprop Lprop 
        | Neg Lprop 
     
data Tableaux = Hoja [Lprop] 
        | Alpha [Lprop] Tableaux 
        | Beta [Lprop] Tableaux Tableaux


esLiteral :: Lprop -> Bool
esLiteral (VarP a) = True
esLiteral (Neg a) = esLiteral a
esLiteral T = True
esLiteral F = True
esLiteral _ = False

literales  = and . map esLiteral 




{-
literales :: [Lprop] -> Bool
literales [] = True
literales ((VarP a):xs) = literales xs
literales (T : xs) = literales xs
literales (F : xs) = literales xs
literales (Neg (T) : xs) = literales xs
literales (Neg (F) : xs) = literales xs 
literales (Neg (VarP a) : xs) = literales xs
literales (_: xs) = False
-}

nextF :: [Lprop] ->  Lprop
nextF _ = error "implementar"

alpha :: Lprop -> Bool
alpha _ = error "implementar"


beta :: Lprop -> Bool
beta _ = error "implementar"

sigma :: Lprop -> Bool
sigma _ = error "implementar"

expAlpha :: [Lprop] -> Lprop -> [Lprop]
expAlpha l f@(Conj a b) = a : b : ln
                            where ln = quita f n

expBeta :: [Lprop] -> Lprop -> ([Lprop], [Lprop])
expBeta _ _ = error "implementar"












