raizes :: (Ord a, Floating a) => a -> a -> a -> [a]
raizes a b c|delta > 0 = let r = sqrt delta
                         in [(-b+r)/(2*a), (-b-r)/(2*a)]
            |delta == 0 = [-b/(2*a)]
            |otherwise = []
            where delta = b^2 - 4*a*c


raiz :: (Ord a, Floating a) => a -> a -> a -> [a]
raiz a b c|delta > 0 = [(-b+ sqrt delta)/(2*a), 
                         (-b - sqrt delta)/(2*a)]
            |delta ==0 = [-b/(2*a)]
            |otherwise = []
            where delta = b^2 - 4*a*c
    
--contagem :: Integral a => p -> [a]
--contagem a = [a|a <- [1..10], odd a]



raizSegGrau :: Double -> Double -> Double -> (Double, Double)
raizSegGrau a b c = ((-b -(sqrt(b^2 -4*a*c)))/(2*a), (-b + (sqrt(b^2 -4*a*c)))/(2*a) )

raize :: Double -> Double -> Double -> (Double, Double)
raize a b c = (x1,x2)
            where 
                x1 = (-b + sqrt delta) / (2*a)
                x2 = (-b - sqrt delta) / (2*a)
                delta = b^2 - 4*a*c