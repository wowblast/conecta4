module Ganador
      ( ganoh,  -- ver si gano horizontal
        ganohv, -- ver si gano  vertical  
        ganoDiag1 -- ver si gano en diagonal
      ) where



--metodo que verifica si gano alguien en forma horizontal
ganoh::[[Int]]->Int->Bool
ganoh [] n = False
ganoh (x:xs) n 
          | seguirh x 0 n = True
          |otherwise = ganoh xs n 
--metodo que verifica e una fila si gano  (tablero-Indice-Valor de jugador ("1 o 2") - true o false)          
seguirh::[Int]->Int->Int->Bool
seguirh l@(x:xs) ind v 
             | ind >= (length l) = False 
             | (ld l ind v )+(li l ind  v )-1>=4 = True
             | otherwise = seguirh l (ind+1) v
          
--suma hacia la derecha si hay en fial seguida
ld::[Int]->Int->Int->Int
ld xs n v
      | n >= length xs = 0
      | xs !! n ==v = 1+ ld xs (n+1) v      
      |otherwise = 0  
--suma hacia la izquierda si hay en fila seguida
li::[Int]->Int->Int->Int
li xs n v
      | n ==0 && (xs !! n ==v) = 1
      | n ==0 && (xs !! n /=v) = 0
      | xs !! n ==v=  1+ li xs (n-1) v      
      |otherwise = 0  

--metodo que verifia si gano en forma vertical
ganohv::[[Int]]->Int->Bool
ganohv [] _ = False
ganohv lis@(x:xs) v 
           | (lv lis 0 v) >=4 = True
           | otherwise = ganohv xs v
          
--metodo que verifica e una columna si gano  (tablero-Indice-Valor de jugador ("1 o 2") - true o false)  
lv::[[Int]]->Int->Int->Int
lv [] ind v = 0
lv (x:xs) ind v 
            | (x !! ind) ==v = 1+ (lv xs ind v ) 
            |otherwise = 0           


           
--metodo que verifica si gano en vertical  hacia la derecha
ganoDiag1::[[Int]]->Int->Bool
ganoDiag1 [] _ = False
ganoDiag1 (x:xs) v 
               | maximum (alldiag (x:xs) 0 v)>=4 ||maximum (alldiagiz (x:xs) 0 v)>=4 = True
               |otherwise =ganoDiag1 xs v


--verifica posibilidades de diagonal hacia la derecha 
alldiag::[[Int]]->Int->Int->[Int]
alldiag [] _ _ = []
alldiag (x:xs)  ind v
                |(length x)>= ind=  [vd (x:xs) ind v]++(alldiag (x:xs) (ind +1) v)
                |otherwise = []

--verifica posibilidades de diagonal hacia la izquierda 
alldiagiz::[[Int]]->Int->Int->[Int]
alldiagiz [] _ _ = []
alldiagiz (x:xs)  ind v
                |(length x)> ind=  [vi (x:xs) ind v]++(alldiagiz (x:xs) (ind +1) v)
                |otherwise = []

--verifica si hay una diagonal hacia la derecha 
vd::[[Int]]->Int->Int->Int
vd [] _ _ = 0
vd (x:xs) ind v 
            | (length x)<= ind = 0
            | (x !! ind) == v = 1 +(vd xs (ind+1) v)
            |otherwise = 0
--verifica si hay una diagonal hacia la izquierda
vi::[[Int]]->Int->Int->Int
vi [] _ _ = 0
vi (x:xs) ind v 
          |  ind<0 = 0
          | (x !! ind) == v = 1 +(vi xs (ind-1) v)
          |otherwise = 0

