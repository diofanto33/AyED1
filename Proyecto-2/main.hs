-- ##################
-- ##  PROYECTO 2  ##
-- ##################

data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq, Bounded, Ord)

-- Ejercicio 2b funcion titulo 

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar  deriving (Show)

data Area = Administrativa | Ensenanza | Economica | Postgrado  deriving (Show, Eq)

data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso  deriving  (Show)

-- Ejercicio 3c

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c = if es_Docente x c then 1 + cuantos_doc xs c else cuantos_doc xs c 

-- Ejercicio 3d de dos maneras usando filter

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' xs c = length (filter (`es_Docente`c) xs)

cuantos_doc'' :: [Persona] -> Cargo -> Int
cuantos_doc'' xs c = length (filter (\ y -> es_Docente y c) xs)

lista_1 = [Decane,Docente Titular,NoDocente Economica, Estudiante Matematica 2020, Docente Asociado, Docente Titular] 

es_Docente :: Persona -> Cargo -> Bool
es_Docente (Docente Titular) Titular = True
es_Docente (Docente Asociado) Asociado = True
es_Docente (Docente Adjunto) Adjunto = True
es_Docente (Docente Auxiliar) Auxiliar = True
es_Docente _ _ = False

division' :: Int -> Int -> Maybe Int
division' _ 0 = Nothing
division' x y = Just(x `div` y) 

-- Ejercicio 4a

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (head xs)

data Cola = VaciaC | Encolada Persona Cola deriving (Show)

-- Ejercicio 5a1

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ c) = Just (c)

-- Ejercicio 5a2

encolar :: Persona -> Cola -> Cola
encolar p VaciaC = (Encolada p VaciaC)
encolar p (Encolada q c) = (Encolada q (encolar p c))

-- Ejercicio 5a3  

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC c = Nothing
busca (Encolada p k) c = if es_Docente p c then Just(p) else busca k c


listaC = (Encolada (NoDocente Economica) (Encolada Decane (Encolada (Docente Adjunto) (Encolada (NoDocente Economica) (Encolada (Docente Titular) VaciaC)))))

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show,Eq)

type Diccionario = ListaAsoc String  String

type Padron = ListaAsoc Int String

-- ¿Como se debe instanciar el tipo ListaAsoc para representar la informacion almacenada en una guia telefonica?
type Guia = ListaAsoc String Int

guia1 = (Nodo "Diego" 122 (Nodo "Maria" 156 (Nodo "Lucas" 444 (Nodo "Cielo" 111 (Nodo "Juanpa" 343 Vacia)))))

-- donde Guia toma un String (Nombre de la persona) y un Int (numero de telefono)

-- 6b1 Programar la funcion la_long :: ListaAsoc a b -> Int que devuelve la cantidad de datos en una lista

lista_g = (Nodo "Hola" 1 (Nodo "Chao" 0 Vacia))


la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo i j xs) = 1 + la_long xs

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b 
la_concat Vacia xs = xs
la_concat ys Vacia = ys 
la_concat (Nodo i j xs) (Nodo w k ys) = (Nodo i j (la_concat xs (Nodo w k ys)))

-- Nodo 1 2 (Nodo 3 1 Vacia)

-- 6b3 la_pares :: ListaAsoc a b -> [(a, b)] que transforma una lista de asociacion en una lista de pares clave-dato.

la_pares :: ListaAsoc a  b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo i j xs) = (i,j):(la_pares xs)

-- la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b 
--- que dada una lista y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve Nothing.

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia k = Nothing
la_busca (Nodo i j xs) k = if k==i then Just(j) else la_busca xs k


--- la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b 
-- que  dada una clave a elimina la entrada en la lista

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar k Vacia = Vacia
la_borrar k (Nodo i j xs) = if i==k then xs else (Nodo i j (la_borrar k xs))


-- 7 *

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving(Show,Eq)

type Prefijos = Arbol String 

-- can, cana, canario, canas, cant, cantar, canto :: Prefijos 

-- can = Rama cana "can" cant 
-- cana = Rama  canario 'a' canas 
-- canario = Rama  Hoja "rio" Hoja 
-- canas = Rama Hoja "s" Hoja 
-- cant = Rama cantar "t" canto 
-- cantar = Rama Hoja "ar" Hoja 
-- canto = Rama Hoja 'o' Hoja

-- a_long :: Arbol a -> Int que dado un  ́arbol devuelve la cantidad de datos almacenados.

a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama i k j) = a_long i + a_long j + 1

-- a_hojas :: Arbol a -> Int que dado un  ́arbol devuelve la cantidad de hojas

a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama i k j) = (a_hojas i) + (a_hojas j)

arbol1 = Rama (Rama 
                   (Rama 
                        Hoja
                    7  
                           (Rama 
                                Hoja 
                           6 
                                    (Rama 
                                         Hoja 
                                    3 
                                             Hoja)))
                    2 Hoja)
            1             (Rama 
                               Hoja 
                          0 
                                Hoja 
               )


-- Funcion que cuenta la cantidad de Ramas de un arbol a_ramas

a_ramas :: Arbol a -> Int
a_ramas Hoja = 0
a_ramas (Rama i k j) = 1 + a_ramas i + a_ramas j

-- a_inc :: Num a => Arbol a -> Arbol a que dado un  ́arbol que contiene numeros,los incrementa en uno.

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama i k j) = (Rama (a_inc i) (k+1) (a_inc j))

-- a_map :: (a -> b) -> Arbol a -> Arbol b  
-- que dada una funcion y un arbol, devuele el arbol con la misma estructura, que resulta de 
-- aplicar la funcion a cada uno de los elementos del arbol. 
-- Revisa la defincion de la funcion anterior y reprogramala usando a_map

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama i k j) = (Rama (a_map f i) (f k) (a_map f j))

a_inc' :: Num a => Arbol a -> Arbol a
a_inc' arbolgenerico = a_map (+1) arbolgenerico
