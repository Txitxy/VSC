--anoBissexto :: Int -> Bool
--anoBissexto n = ( mod ) n 4 == 0 && (( mod )) n 100 / = 0 || (mod) n 400 == 0)


type Date = (Int, Int, Int)

criaData :: Date -> Bool
criaData (d, m, a)|d >= 1 && d <= 31 && (m == 1 ||m == 3 ||m == 5 || m == 7 ||m == 8 ||m == 10 ||m == 12) = True
                  |d >= 1 && d <= 30 && (m == 4 || m == 6 ||m == 9 ||m == 11 ) = True
                  |d >= 1 && d <= 28 &&  m == 2 && not bissexto_ano = True
                  |d >= 1 && d <= 29 &&  m == 2 && bissexto_ano = True
                  |otherwise = False
                  where 
                      bissexto_ano|mod a 400 == 0 = True
                                  |mod a 4 == 0 && mod a 100 /= 0 = True
                                  |otherwise = False 





biss :: Integral a => a -> String
biss ano |mod ano 4 == 0 = "Ano Bissexto"   
         |mod ano 400 == 0 && mod ano 100 /= 0 = "Ano Bissexto."
         |otherwise = "Nao e Bissexto"


fatorial :: Int -> Int 
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

teste :: [Char] -> Bool
teste ['a',_,_] = True
teste _ = False 


tercei :: [a] -> [a]
tercei xs = tail xs
{- -biss :: Integral a => [a] -> [a]  --[Int] -> [Int]
biss n = [x| x <- n, bisse x]
           where 
               bisse ano|mod ano 400 == 0 = True
                        |mod ano 4 == 0 && mod ano 100 /= 0 = True
                        |otherwise = False -}

{-dat :: (Ord a, Num a) => a -> String
dat e 
data Data = Invalid | Null | Data {dia :: Day, mes :: Month, ano :: Year} deriving (Eq)


type Date = (Int,Int,Int)

cData :: Date -> Bool
cData (d,m,a) | d >= 1 || d < 31 = True
              | m < 1 || m > 12 = True
              | a > 0 || a == 0 = True
              | m == 2 || d <= 29 = True
              | otherwise = False --Data dia = d, mes = m, ano = a 

anoBissexto :: Int -> Bool
anoBissexto x = (mod) x 4 == 0 && ((mod) x 100 /= 0 || (mod) x 400 == 0) && x /= 0 -}