import Data.List
myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

--maxim :: Integer -> Integer -> Integer
--maxim x y = if (x > y) then x  else y

--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)
maxim3 x y z = maxim x (maxim y z)

maxim3 x y z = let u = (maxim x y) in (maxim u z)
   

eeny :: Integer -> String
eeny = undefined

fizzbuzz :: Integer -> String
fizzbuzz = undefined

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci :: Integer -> Integer
tribonacci = undefined

binomial :: Integer -> Integer -> Integer
binomial = undefined

maxim4 a b c d = 
    let
        e = maxim3 a b c
    in
        maxim e d

--6a  o funcție cu doi parametri care calculează suma pătratelor lor; 
suma_patrate :: Integer -> Integer -> Integer
suma_patrate a b = a*a + b*b

--6b  o funcție cu un parametru ce întoarce stringul "par" dacă parametrul este par și "impar" altfel;
paritate :: Integer -> String
paritate x
    | x `mod` 2 == 0 = "par"
    | otherwise      = "impar"


--6c o funcție care calculează factorialul unui număr;
factorial :: Integer -> Integer
factorial x
    | x == 0    = 1
    | x == 1    = 1
    | otherwise = x*factorial(x-1)


--6d o funcție care verifică dacă primul parametru este mai mare decât dublul celui 
--de-al doilea parametru;
mai_mare_decat_dublul ::  Integer -> Integer -> Bool
mai_mare_decat_dublul a b
    | a > b*2   = True
    | otherwise = False

--6 e  o funcție care calculează elementul maxim al unei liste.

maxim_lista :: (Ord a) => [a] -> a
maxim_lista [a] = a
maxim_lista (h:t)
    | h > maxT  = h
    | otherwise = maxT
  where maxT = maxim_lista t


--7 Scrieți o funcție poly cu patru argumente de tip Double (a,b,c,x) care calculează
-- a*x^2+b*x+c. Scrieți și signatura funcției (poly :: ??).
poly :: Double -> Double -> Double -> (Double ,Double)
poly a b c = (x1,x2)
  where 
    x1 = (-b-sqrt(b*b-4*a*c)) / (2*a) 
    x2 = (-b+sqrt(b*b-4*a*c)) / (2*a)

--8 Scrieți o funcție eeny care întoarce stringul "eeny" atunci când primește ca input 
--un număr par și "meeny" când primeste ca input un număr impar. Hint: puteți folosi funcția even
eeny2 :: Int->String
eeny2 a 
    | even(a) = "eeny"
    | otherwise  = "meeny"

--9 Scrieți o funcție fizzbuzz care întoarce "Fizz" pentru numerele divizibile cu 3, 
--"Buzz" pentru numerele divizibile cu 5 și "FizzBuzz" pentru numerele divizibile cu ambele.
-- Pentru orice alt număr întoarce șirul vid. Scrieți două definiții pentru funcția fizzbuzz: una 
--folosind if și una folosind gărzi (condiții). Hint: pentru a calcula restul împărțirii unui număr 
--la un alt număr puteți folosi funcția mod.
fizzbuzz2 :: Integer -> String
fizzbuzz2 a
    | (a `mod` 3 == 0) && (a `mod` 5 == 0) = "FizzBuzz"
    | a `mod` 3 == 0 = "FIZZ"
    | a `mod` 5 == 0 = "Buzz"
    | otherwise =""

--10 Implementați funcția tribonacci dând o definiție bazată pe cazuri și una ecuațională, cu șabloane.
tribonacci2 :: Integer -> Integer
tribonacci2 a
    |a == 1 = 1
    |a == 2 = 1
    |a == 3 = 3
    |otherwise = tribonacci2(a-1)+tribonacci2(a-1)+tribonacci2(a-3)

--11 Scrieți o funcție recursivă care calculează coeficienții binomiali. 
--Coeficienții sunt determinați folosind următoarele ecuații (pentru orice întregi n, k, astfel încât 1 ≤ k < n):
binomial2 :: Integer -> Integer -> Integer
binomial2 a b
    | a == 0 = 0
    | b ==  0 = 1
    | otherwise = binomial2(a-1) b + binomial2(a-1) (b-1)