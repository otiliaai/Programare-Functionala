import Data.Char (digitToInt)

--1 a) verifL - verifică dacă lungimea unei liste date ca parametru este pară.

verifL :: [Int] -> Bool
verifL l
    | length(l) `mod` 2 == 0 = True
    | otherwise = False

--1 b) takefinal - pentru o listă l dată ca parametru și un număr n, întoarce o listă 
--care conține ultimele n elemente ale listei l. 
--Dacă lista are mai puțin de n elemente, întoarce lista nemodificată.

takefinal :: [Int] -> Int -> [Int]
takefinal  t n 
    | length(t) < n = t
    | otherwise =  drop (length(t)-n) t


takefinal2 :: String -> Int -> String
takefinal2  t n 
    | length(t) < n = t
    | otherwise =  drop (length(t)-n) t


--1 c) remove - pentru o listă și un număr n, întoarce lista primită ca parametru din care 
--se șterge elementul de pe poziția n. (Hint: puteți folosi funcțiile take și drop). 
remove :: [Int] -> Int -> [Int]
remove l n =
    let l1 = take (n-1) l
        l2 = drop n l
    in l1++l2

--2
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t

 --2 a) myreplicate - pentru un întreg n și o valoare v, întoarce lista ce conține 
 --n elemente egale cu v. Să se scrie și prototipul funcției.
myreplicate :: Int -> Int -> [Int]
myreplicate n v
    | n == 0 = []
    | otherwise = v : myreplicate (n-1) v

--2 b) sumImp - pentru o listă de numere întregi, calculează suma elementelor impare. 
--Să se scrie și prototipul funcției.
sumImp :: [Int] -> Int 
sumImp [] = 0
sumImp (h:t) 
    | even(h) =  sumImp(t)
    | otherwise = sumImp(t) + h


--2 c) totalLen - pentru o listă de șiruri de caractere, calculează suma lungimilor 
--șirurilor care încep cu caracterul 'A'.
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | head(h) == 'A' = totalLen(t) + length(h)
    | otherwise  = totalLen(t)


--3) Scrieți o funcție nrVocale care primește ca parametru o listă de șiruri de caractere 
--și calculează numărul total de vocale din șirurile palindrom. Pentru a verifica dacă un șir
--e palindrom, puteți folosi funcția reverse, iar pentru a căuta un element într-o listă, 
--puteți folosi funcția elem. Puteți defini funcții auxiliare.

palindrom :: String -> Bool
palindrom s
    | s == reverse(s) = True
    | otherwise = False

vocale :: String -> Int
vocale [] = 0
vocale (h:t)
    | h `elem` "aeiouAEIOU" = 1 + vocale(t)
    | otherwise  = vocale(t)


nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (cuv:t)
    | palindrom(cuv) = vocale(cuv) + nrVocale(t)
    | otherwise = nrVocale(t)

--4) Scrieți o funcție care primește ca parametri un număr și o listă de întregi și adaugă numărul dat după fiecare element par din listă. 
f :: Int -> [Int] -> [Int]
f n [] = []
f n (h:t)
    | even(h) = h : n : f n t
    | otherwise = h : f n t



semiPareComp2 :: [Int] -> [Int]
semiPareComp2 l = [ x `div` 2 | x <- l, even x ]

--5) Scrieți o funcție care determină lista divizorilor unui număr întreg primit ca parametru.
divizori :: Int -> [Int]
divizori n = [ x | x <- [1..n], n `mod` x == 0]

--6) Scrieți o funcție care primește ca parametru o listă de numere întregi și întoarce lista listelor de divizori.
listadiv :: [Int] -> [[Int]]
listadiv l = [ divizori(x) | x <- l ]

--7) Scrieți o funcție care primește ca parametri:
--două numere întregi ce reprezintă limita inferioară și cea superioară a unui interval 
--închis și o listă de numere întregi și întoarce numerele din listă ce aparțin intervalului.
inIntervalComp li ls l = [ x |x <- l, x>=li && x<=ls ]

inIntervalRec li ls [] = []
inIntervalRec li ls (h:t)
    |  h>=li && h<=ls = h : inIntervalRec li ls t
    | otherwise = inIntervalRec li ls t

--8) Scrieți o funcție care numără câte numere strict pozitive sunt într-o listă dată ca argument.
pozitiveRec ::  [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1 + pozitiveRec(t)
    | otherwise = pozitiveRec(t)


pozitiveCreare l = [ x | x <- l , x > 0]
pozitiveComp l = length(pozitiveCreare(l))

--9) Scrieți o funcție care întoarce lista pozițiilor elementelor impare dintr-o listă 
--de numere primită ca parmetru. 
pozitie l = zip [0..(length(l)-1)] l
pozitiiImpare l = [ x | (x,y) <- pozitie(l) , y `mod` 2 == 1]

pozimp :: [Int] -> Int -> [Int]
pozimp [] _ = []
pozimp (h1:t1) p 
    | h1 `mod` 2 == 1 = p : pozimp t1 (p+1)
    | otherwise = pozimp t1 (p+1)

pozitiiImpare2 l = pozimp(l) 0

--10) Scrieți o funcție care calculează produsul tuturor cifrelor care apar într-un șir de 
--caractere primit ca parametru. Dacă șirul nu conține cifre, funcția întoarce 1 . 

iscifra :: String -> [Int]
iscifra prop = [ digitToInt x | x <- prop , x `elem` "1234567890"]

produs [] = 1
produs (h:t) = h*produs(t)

multDigitsComp str = produs(iscifra(str))

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (h:t)
    | h `elem` "0123456789" = (digitToInt h)*multDigitsRec(t)
    | otherwise = multDigitsRec(t)

--11) Scrieți o funcție care primește ca argument o listă și întoarce toate permutările ei.

permutare l = [ (drop i l) ++ (take i l)| i <- [0..length(l)-1] ]

--12) Scrieți o funcție care primește ca argument o listă și un număr întreg k, 
--și întoarce toate combinările de k elemente din listă.

combinari 0 _ = [[]] 
combinari _ [] = []
combinari k (h:t) = [ h:ys | ys <- combinari (k-1) t] ++ combinari k t

--13) Scrieți o funcție care primește ca argument o listă și un număr întreg k, și întoarce 
--toate aranjamentele de k elemente din listă.

aranjamente 0 _ = [[]] 
aranjamente _ [] = []
aranjamente k t = [ h : t2 | h <-t, t2 <- aranjamente (k-1) (filter (/=h) t)]

--14) Scrieți o funcție care primește ca argument un număr întreg ce reprezintă '
--dimensiunea unei table de șah și un numar întreg ce reprezintă numărul de dame ce
-- trebuie așezate pe tablă, și întoarce lista pozițiilor în care pot fi așezate damele fără să se atace.

-- o damă este reprezentată ca (linie, coloană)
type Pozitie = (Int, Int)

-- funcția principală
dame :: Int -> Int -> [[Pozitie]]
dame n k = place k [] [1..n]

-- funcție auxiliară care plasează recursiv damele
place :: Int -> [Pozitie] -> [Int] -> [[Pozitie]]
place 0 acc _  = [acc]  -- am așezat toate damele
place _ acc [] = []      -- nu mai sunt rânduri disponibile
place k acc (r:rs) =
    [ rez | c <- [1..n], isSafe (r,c) acc, rez <- place (k-1) ((r,c):acc) rs ]
  where n = length (r:rs) + length acc  -- reconstruim dimensiunea tablei

-- verifică dacă o nouă damă nu se atacă cu cele deja puse
isSafe :: Pozitie -> [Pozitie] -> Bool
isSafe (r,c) dameExistente =
    all (\(r2,c2) -> c /= c2 && abs (r - r2) /= abs (c - c2)) dameExistente

