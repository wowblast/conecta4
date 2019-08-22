module Store
      ( Store,
        initialm,
        mostrar,    
        updatem,        
        buscarcolumna,    
         ) where


data Store = Sto [[Int]]

--inicializa la matriz
initialm:: Store
initialm  = Sto [listacero 7|x<-[1..6]]
--muestra la matriz
mostrar::Store->[[Int]]
mostrar (Sto xs) = xs

--genera lista de ceros 
listacero::Int->[Int]
listacero 0 = []
listacero n = 0:(listacero (n-1))



--busca la fila 
updatem :: [[Int]] -> Int -> Int ->Int-> [[Int]]
updaten  [] _ _ _= []
updatem ((x:xs)) 1 y n = [(mdf x y n)]++xs
updatem j@( (a:xs)) x y n = [a]++updatem  xs (x-1) y n
 
--busca la columa y remplaza el dato
mdf::[Int]->Int->Int->[Int]
mdf (x:xs) 1 n = n : xs
mdf (x:xs) c n = x:(mdf (xs) (c-1) n )
              

--selecciona la comumna a depositar 
buscarcolumna::[[Int]]->Int->Int
buscarcolumna []  _ = 0
buscarcolumna lis@(x:xs) n
                  | buscarvacio x n  = length lis
                  |otherwise = buscarcolumna xs n 

--busca si existe  vacio en la columna
buscarvacio::[Int]->Int->Bool
buscarvacio [] _ = False
buscarvacio (x:xs) 1 
                | x==0 = True
                |otherwise = False
buscarvacio (x:xs) n = buscarvacio xs (n-1)



