{-# OPTIONS_GHC #-}
module Menu(
menu --inicia el juego
)where
import Store
import Ganador
import Exceptions 	
import System.Random
-- menu de juego		
menu  = do
		
		putStrLn " -------------- MENU CONECTA 4 -------------"
		putStrLn " 1. 2 Jugadores (PVP)"
		putStrLn " 2. Jugar contra AI (PVE)"
		putStrLn " 3. Terminar"
		putStrLn " ---------------------------------"
		putStr " Ingrese una opcion: "
		x <- getint
		let aa = initialm  
                   
		case x of
			1 -> do
				
				mapM_ print (mostrar aa)
				putStrLn "Juego empezo"
				juego (mostrar aa) 1	
					 
			2 -> do 
				g <- randomRIO (1,7)
				let j1 = vv1 g
				let tablero = mostrar aa
				let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j1) j1 2
				mapM_ print [[1..7]]
				mapM_ print (nuevo)
				putStrLn "Juego empezo"
				juegovsAi (nuevo) j1
				   
			3 -> return () ;
			_   -> do
				   putStrLn "entrada incorrecta"
				   menu	   
--funcion recursiva para el juego contra  AI
juegovsAi tablero jia = do
					putStrLn " Jugador 1 ingrese fila  de 1 al 7" 
					j1<-getint
					if (length (head tablero)>=j1&&j1>0)
							then do 
									if ( columnafull (buscarcolumna (reverse tablero ) j1))
											then do 
												putStrLn "Columna llena intente de nuevo\n"
												juegovsAi tablero jia
											else do 									  	       								
													let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j1) j1 1
													if ((ganoh nuevo 1) ||( ganohv nuevo 1)||(ganoDiag1 nuevo 1))
															then do
																mapM_ print [[1..7]]
																mapM_ print ( nuevo)
																putStrLn " gano jugador 1 Felicidades\n"
																menu 
													else if ( matrizfull nuevo)  									 	 
															then do
																putStrLn "Empate "
																menu									  
													else do
														mapM_ print [[1..7]]
														mapM_ print ( nuevo);
														putStrLn "\njugada de la ai\n"
														jugadaia nuevo  jia j1
					else do 
						putStrLn "Columna no existente intente de nuevo\n"
						juego tablero 1								   


vv1::Int->Int
vv1 a = a
--jugada de la ia segun la jugada de jugador
jugadaia tablero jia  j1 = do 
					   vv <- randomRIO (1,4 ) 
					   let lookupVal = 0+(vv1 vv)
					   case lookupVal of

						1 -> do
							  rellenarlado tablero jia

						2 -> do
							  rellenararriba tablero jia
						
						3 -> do
							  bloquear tablero j1	
						4 -> do 
							  bloquearlado tablero j1
						
					    	    
--rellna una casilla de lateral o diagonal						      	
rellenarlado tablero jia = do 
								v <-randomRIO ((-1), 1) 
								let j1 =  jia + (vv1 v)
									
								if (length (head tablero)>=j1&&j1>0)
									then do 
											if ( columnafull (buscarcolumna (reverse tablero ) j1))
													then aleatorio tablero 
													else do 									  	       								
															let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j1) j1 2
															if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
																	then do
																		mapM_ print [[1..7]]
																		mapM_ print ( nuevo)
																		putStrLn " gano la AI Felicidades\n"
																		menu 
															else if ( matrizfull nuevo)  									 	 
																	then do
																		putStrLn "Empate "
																		menu									  
															else do
																mapM_ print [[1..7]]
																mapM_ print ( nuevo);
																juegovsAi nuevo j1
								else do 
									aleatorio tablero											   
--relena arriba de lo que la ai hizo en la anterior jugada	 
rellenararriba tablero jia = do
								let j2 = jia								 
								if ( columnafull (buscarcolumna (reverse tablero ) j2))
											then aleatorio tablero
											else do 									  	       								
													let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j2) j2 2
													if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
																then do
																	    mapM_ print [[1..7]];
																		mapM_ print ( nuevo);
																		putStrLn " gano jugador AI Felicidades\n";
																		menu 
																else if ( matrizfull nuevo)  									 	 
																		then do
																			putStrLn "Empate "
																			menu									  
																else do
																	mapM_ print [[1..7]]
																	mapM_ print ( nuevo);
																	juegovsAi nuevo j2
--bloquea al jugador de forma vertical								
bloquear tablero jugador = do
							let j2 = jugador
							if ( columnafull (buscarcolumna (reverse tablero ) j2))
													then aleatorio tablero
													else do 									  	       								
															let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j2) j2 2
															if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
																then do 
																		mapM_ print [[1..7]]
																		mapM_ print ( nuevo)
																		putStrLn " gano jugador Ai  Felicidades\n"
																		menu 
															else if ( matrizfull nuevo)  									 	 
																	then do
																		putStrLn "Empate "
																		menu									  
															else do
																mapM_ print [[1..7]]
																mapM_ print ( nuevo);
																juegovsAi nuevo j2

--bloquea al jugador de forma vertical								
bloquearlado tablero jugador = do
	v <-randomRIO ((-1), 1) 
	let j2 =  jugador + (vv1 v)
	
	if ( columnafull (buscarcolumna (reverse tablero ) j2))
							then aleatorio tablero
							else do 									  	       								
									let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j2) j2 2
									if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
										then do 
												mapM_ print [[1..7]]
												mapM_ print ( nuevo)
												putStrLn " gano jugador Ai  Felicidades\n"
												menu 
									else if ( matrizfull nuevo)  									 	 
											then do
												putStrLn "Empate "
												menu									  
									else do
										mapM_ print [[1..7]]
										mapM_ print ( nuevo);
										juegovsAi nuevo j2


--funcion de poner una ficha en forma aleatoria caso que ocurra un eroor en la jugada de la ai							
aleatorio tablero = do
						 j2 <- randomRIO (1,7)
						 if ( columnafull (buscarcolumna (reverse tablero ) j2))
												then  aleatorio tablero
													
													
												else do 									  	       								
														let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j2) j2 2
														if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
																then do
																	mapM_ print [[1..7]]
																	mapM_ print ( nuevo)
																	putStrLn " gano jugador AI Felicidades\n"
																	menu 
														else if ( matrizfull nuevo)  									 	 
																then do
																	putStrLn "Empate "
																	menu									  
														else do
															mapM_ print [[1..7]]
															mapM_ print ( nuevo);
															juegovsAi nuevo j2															



--funcion recursiva que se usa para poner en marcha el juego de 2 personas
juego tablero n = do
				  case n of  
				   1-> do 
						putStrLn " Jugador 1 ingrese fila  de 1 al 7" 
						j1<-getint
						if (length (head tablero)>=j1&&j1>0)
							    then do 
										if ( columnafull (buscarcolumna (reverse tablero ) j1))
												then do 
													putStrLn "Columna llena intente de nuevo\n"
													juego tablero 1
												else do 									  	       								
														let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j1) j1 1
														if ((ganoh nuevo 1) ||( ganohv nuevo 1)||(ganoDiag1 nuevo 1))
																then do
																	mapM_ print [[1..7]]
																	mapM_ print ( nuevo)
																	putStrLn " gano jugador 1 Felicidades\n"
																	menu 
														else if ( matrizfull nuevo)  									 	 
																then do
																	putStrLn "Empate "
																	menu									  
														else do
															mapM_ print [[1..7]]
															mapM_ print ( nuevo);
															juego nuevo 2
						else do 
							putStrLn "Columna no existente intente de nuevo\n"
							juego tablero 1												  
				   2 -> do 
						putStrLn " Jugador 2 ingrese fila  de 1 al 7" 
						j2<-getint
						if (length (head tablero)>=j2&&j2>0)
								then do 
										if ( columnafull (buscarcolumna (reverse tablero ) j2))
												then do 
													putStrLn "Columna llena intente de nuevo\n"
													juego tablero 2
												else do 									  	       								
														let nuevo =updatem ( tablero) (buscarcolumna (reverse tablero ) j2) j2 2
														if ((ganoh nuevo 2) ||( ganohv nuevo 2)||(ganoDiag1 nuevo 2))
																then do
																	mapM_ print [[1..7]]
																	mapM_ print ( nuevo)
																	putStrLn " gano jugador 2 Felicidades\n"
																	menu 
														else if ( matrizfull nuevo)  									 	 
																then do
																	putStrLn "Empate "
																	menu									  
														else do
															mapM_ print [[1..7]]
															mapM_ print ( nuevo);
															juego nuevo 1
						else do 
							putStrLn "Columna no existente intente de nuevo\n"
							juego tablero 2	
															

--funcion que obtiene el numero entero  de la consola									  
getint :: IO Int
getint = readLn										  

								
					