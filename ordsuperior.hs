import Prelude hiding ((.))

duas_vezes :: (a -> a) -> a -> a
duas_vezes f x = f(f x)

adicionar :: Int -> Int -> Int
adicionar x y = x + y

map' :: (a -> b) -> [a] -> [b]
map' f []=[]
map' f (x:xs) = f x : map' f xs


(.) :: (b->c) -> (a->b) -> (a->c)
f.g = \ x -> f(g x)

odd' :: Int -> Bool
odd' = not.even

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs


decr  ::  Int  -> [ Int ] -> [ Int ]
decr n lista =  filter ( > n) lista


{- map f xs = [fx|x<-xs] (por compreensão) função map aplica uma condição para cada elemento de uma lista

ex.: map (+1) [1,3,5,7] ->  >[2,4,6,8] 

filter filtra elementos ex:
filter odd [1..10]  >[1,3,5,7,9]

Por compreensao Filter
filter p xs = [x|x <- xs, p x] 

por recursividade:
 
               
foldl(*) 1[1,2,3,4]
>24 


odd n = not (even n)
twice f x = f(fx)
sumsqreven ns = sum(map(^2)(filter even ns))

odd = not.even
twice f = f.f
sumsqreven = sum.map(^2) -}
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)| p x = x:filter' p xs
               |otherwise = filter' p xs