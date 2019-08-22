module Exceptions(
columnafull,
matrizfull

)where


--verifica columna llena 

columnafull::Int->Bool
columnafull n 
            | n==0 = True
            |otherwise = False 

--verica si la matriz se lleno 
matrizfull::[[Int]]->Bool
matrizfull [] = True
matrizfull (x:xs) = (filallena x)&&(matrizfull xs) 

--verifica fila llena 
filallena::[Int]->Bool
filallena [] = True
filallena (x:xs)
            | x/=0 = True && filallena xs
            |otherwise = False