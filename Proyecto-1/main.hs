-- ################
-- ## Proyecto 1 ##
-- ################

--- Ejercicio 1) 
--- Programá las siguientes funciones:

---a) esCero :: Int -> Bool, que verifica si un entero es igual a 0 

esCero :: Int -> Bool
esCero x = (x==0)

---b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0

esPositivo :: Int -> Bool
esPositivo x = x>0

---c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula

esVocal :: Char -> Bool
esVocal x = (x=='a') || (x=='e') || (x=='i') || (x=='o') || (x=='u') 

--- Ejercicio 2)
--- Programa las siguientes funciones usando recursion o composicion:

---a) paratodo :: [Bool] -> Bool,  que  verifica  que todos los  elementos  de  una  lista sean True.

paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = (x==True) && paraTodo xs

---b) sumatoria :: [Int] -> Int,  que  calcula  la  suma  de  todos  los  elementos  de  una lista de enteros.

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

---c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de una la lista de enteros.

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x*(productoria xs)

---d) factorial :: Int -> Int, que toma un numero n y calcula n!

fac :: Int -> Int
fac 0 = 1
fac n = n*(fac (n-1))

---e) Utiliza la funcion sumatoria para definir, promedio :: [Int] -> Int
--- que toma una lista de numeros no vacia y calcula el valor promedio (truncado, usando division entera)

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)

--- Ejercicio 3) 
--- Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se encuentra en una lista

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False 
pertenece n (x:xs) = (n==x) || (pertenece n xs)

--- Ejercicio 4) 
--- Programá las siguientes funciones que implementan los cuantificadores generales. Notá que
--- el segundo parámetro de cada función, es otra función!

--- a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
--- predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.

paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] p = True
paraTodo' (x:xs) p = (p x) && (paraTodo' xs p)

--- b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
---predicado t :: a -> Bool, determina si algún elemento de xs satisface el predicado t.

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] p = False
existe' (x:xs) p = (p x) || (existe' xs p)

--- c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
--- función t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
--- suma de los valores que resultan de la aplicación de t a los elementos de xs.

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = (f x) + (sumatoria' xs f)

--- d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
--- y una función t :: a -> Int, calcula el producto de los valores que resultan de la aplicación de t a los elementos de xs.

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = (f x)*(productoria' xs f)

--- Ejercicio 5)  
--- Definı́ nuevamente la función paraTODO, pero esta vez usando la función paratodo’ (sin recursión ni análisis por casos!)

paraTODO :: [Bool] -> Bool
paraTODO xs = (paraTodo' xs id) 

--- Ejercicio 6)  
--- Utilizando las funciones del ejercicio 4, programá las siguientes funciones por composición,
--- sin usar recursión ni análisis por casos.

--- a) todosPares :: [Int] -> Bool verifica que todos los números de una lista sean pares.

todosPares :: [Int] -> Bool 
todosPares xs = paraTodo' xs even 

--- b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algún número dentro del
--- segundo parámetro que sea múltiplo del primer parámetro.

--- Usando una funcion lambda en su definicion 

hayMultiplo :: Int -> [Int] -> Bool 
hayMultiplo n xs = existe' xs (\x -> mod x 2 == 0)
 

--- c) sumaCuadrados :: Int -> Int, dado un número no negativo n, calcula la suma de los primeros n cuadrados. 

sumaCuadrados :: Int -> Int 
sumaCuadrados n = sumatoria' [0..n] (^2)

--- d) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursión?

facc :: Int -> Int
facc n = productoria' [1..n] id

--- e) multiplicaPares :: [Int] -> Int que calcula el producto de todos los número pares de una lista.

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (filter even xs) id

--- Ejercicio 7) 
--- Indagá en Hoogle sobre las funciones map y filter. También podes consultar su tipo en ghci con el comando :t.

--- ¿Qué hacen estas funciones?

--- map toma una función y una lista y aplica esa función a cada elemento de esa lista, produciendo una nueva lista.

---filter es una función que toma un predicado y una lista y devuelve una lista con los elementos que satisfacen el predicado

--- ¿A qué equivale la expresión map succ [1, -4, 6, 2, -8], donde succ n = n+1?

--- La expresion  map succ [1, -4, 6, 2, -8] = [2, -3, 7, 3, -7]

--- ¿Y la expresión filter esPositivo [1, -4, 6, 2, -8]?

--- filter esPositivo [1, -4, 6, 2, -8] = [1,6,2]

--- Ejercicio 8) 
--- Programá una función que dada una lista de números xs, devuelve la lista que resulta de duplicar cada valor de xs.

--- a) Definila usando recursión

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (2*x):(duplica xs)

--- b) Definila utilizando la función map.

dup :: [Int] -> [Int]
dup xs = map (2*) xs

--- Ejercicio 9)
--- Programá una función que dada una lista de números xs, calcula una lista que tiene como elementos aquellos números de xs que son pares.

--- a) Definila usando recursión.

listPar :: [Int] -> [Int]
listPar [] = []
listPar (x:xs) | even x == True = x:listPar xs
               | otherwise = listPar xs 

--- b)  Definila utilizando la función filter.

listPar' :: [Int] -> [Int]
listPar' xs = filter even xs

--- c) Revisá tu definición del ejercicio 6e. ¿Cómo podes mejorarla?

multiPar :: [Int] -> Int
multiPar xs = productoria' (listPar' xs) id

--- O tambien usando la funcion product del prelude

multiPar' :: [Int] -> Int
multiPar' xs = product (listPar' xs)

--- Ejercicio 10) 
--- La función primIgualesA toma un valor y una lista, y calcula el tramo inicial más largo de la lista cuyos elementos son iguales a ese valor.
 
 --- a)Programá primIgualesA por recursión                     

primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA n [] = []
primIgualesA n (x:xs) | (n==x) = x:(primIgualesA n xs)
                      | otherwise = primIgualesA n []

--- b) Programá nuevamente la función utilizando takeWhile

--- Segun Hoogle:
--- takeWhile, applied to a predicate p and a list xs, returns the longest prefix (possibly empty) of xs of elements that satisfy p.

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (n==) xs  

--- Ejercicio 11) 
--- La función primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre sı́.

--- a) Programá primIguales por recursión

primIguales :: (Eq a) => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) | (x == y) = x:primIguales (y:xs)
                     | otherwise = [x]

--- b) Usá cualquier versión de primIgualesA para programar primIguales. Está permitido dividir en casos, pero no usar recursión.

--- Entonces uso 10.b y luego: 

primIguales' :: (Eq a) => [a] -> [a]
primIguales' xs = primIgualesA' (xs !! 0) xs 
