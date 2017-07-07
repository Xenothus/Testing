import Data.List
import Data.Map

-- Testowanie zdefiniowanych niżej funkcji:

main = do
			print ("Kolejne zdanie:")
			print (wypisz_zmienne ( (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) ))
			print (drukuj ( (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) ))
			print (sprawdz ( (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) ) [('p',False),('q',True),('r',False)] )
			print (jest_tautologia ( (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) ))
			print ("Kolejne zdanie:")
			print (wypisz_zmienne ( (A (Z 'p') (N (Z 'p'))) ))
			print (drukuj ( (A (Z 'p') (N (Z 'p'))) ))
			print (sprawdz ( (A (Z 'p') (N (Z 'p'))) ) [('p',False)] )
			print (jest_tautologia ( (A (Z 'p') (N (Z 'p'))) ))
			print ("Testowanie funkcji kolejny_ciag:")
			print (kolejny_ciag [])
			print (kolejny_ciag [False,False,False])
			print (kolejny_ciag [True,True,False,True])
			print (kolejny_ciag [True,True,True,True,True])
			print ("Testowanie funkcji generuj_sam_falsz:")
			print (generuj_sam_falsz 0)
			print (generuj_sam_falsz 1)
			print (generuj_sam_falsz 4)
			print ("Testowanie funkcji czy_sama_prawda:")
			print (czy_sama_prawda [])
			print (czy_sama_prawda [True,True,False])
			print (czy_sama_prawda [True,True])
			print ("Testowanie funkcji kolejny_ciag:")
			print (kolejny_ciag [])
			print (kolejny_ciag [False,False,False])
			print (kolejny_ciag [True,True,False,True])
			print (kolejny_ciag [True,True,True,True,True])
			print ("Testowanie funkcji sprawdz_kolejna:")
			print (sprawdz_kolejna (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) [])
			print (sprawdz_kolejna (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r'))) [False,False,False])
			print (sprawdz_kolejna (A (Z 'p') (N (Z 'p'))) [False])
			print(sumOfMax 1000 10)

		
------------ ZADANIE 1.37 -------------		
		
-- Niech d(n) oznacza liczbę dzielników n. M(n,k) będzie maksymalną 
-- 	wartością d(j) for n ≤ j ≤ n+k-1. Napisz funkcję obliczającą S(u,k) 
-- 	będące sumą M(n,k) for 1 ≤ n ≤ u-k+1.Przykładowo S(1000,10)=17176. 


-- Implementacja d(n) - liczba dzielników n.
-- divisorsOfNumber(liczba_dla_ktorej_szukamy_dzielnikow)
-- Sprawdz kazda liczbe x nalezaca do zbioru <1, n/2> i 
-- 	przetestuj warunek (n `mod` x == 0)
-- Zlozonosc O(n/2) = O(n)
divisorsOfNumber :: (Integral a) => a -> a
divisorsOfNumber x = let 
-- loop(wprowadzona_liczba, iterator, liczba_dzielnikow)		
	loop x' i n =
		if(i > (quot x' 2)) then (n + 1)  						
			-- szukanie dzielnikow konczy sie w polowie
			-- wiec nalezy zwiekszyc zwracana wartosc o 1,
			-- poniewaz kazda liczba dzieli sie przez sama siebie
			
		else if(x' `mod` i == 0) then loop x' (i+1) (n+1)
		else loop x' (i+1) (n)
	in loop x 1 0

-- funkcja dodtakowa, wypisuje liczbe dzielnikow kolejnych liczb naturalnych
-- 		moze znajdzie sie jakas zaleznosc?
divList = listOfDivisors 100000000
listOfDivisors :: (Integral a) => a -> [(a,a)]
listOfDivisors n = let
	loop n' i list=
		if(n == i) then reverse list
		else loop n' (i+1) ((i,(divisorsOfNumber i)):list)
	in loop n 1 []

-- implementacja M(n, k)
-- maxValue(punkt_poczatkowy, zakres_przedzialu)		
-- Znajduje liczbe o najwiekszej liczbie dzielnikow w przedziale
--	<n, n+k-1>. Typowy algorytm wyszukujacy maksimum dla zbioru
--	wartosci, dla kazdej liczby wywolywana jest divisorsNumber()
-- Zlozonosc O(n*k) = O(n^2)
maxValue :: (Integral a) => a -> a -> a
maxValue n k = let
-- loop(liczba_n, liczba_k, iterator, 
-- wartosc_maksymalna_dla_iteratora, wartosc_maksymalna)		
	loop n' k' i i_max max = 
		if (i > (n' + k' - 1)) then max
		else
			if(max < i_max) 
				then loop n' k' (i+1) (divisorsOfNumber (i+1)) i_max
			else loop n' k' (i+1) (divisorsOfNumber (i+1)) max
	in loop n k n (divisorsOfNumber n) 0


 -- Implementacja S(u, k)
 -- sumOfMax(zakres_poczatkow_przedzialow, zakres_pojedynczego_przedzialu)
 -- Ustanawia iterator i, ktory jest poczatkiem przedzialu, przesuwa sie 
 -- 	krokowo o 1 (zaczynajac od 1, konczac na u-k+1) znajduje liczbe ktora posiada 
 -- 	najwiecej dzielnikow w kazdym z przedzialow <i, i+k-1> 
sumOfMax :: (Integral a) => a -> a -> a
sumOfMax u k = let
-- loop(liczba_u, liczba_k, iterator, suma, maksimum_w_przedziale)
	loop u' k' i sum max = 
			let newElement = divisorsOfNumber (i+k-1) in
				if (i > (u' - k' + 1)) then (sum + max - 4)
				else 
					if (divisorsOfNumber (i-1) >= max) then 
						if(newElement >= max) then 
							loop u' k' (i + 1) (sum + max) newElement
						else 			
							loop u' k' (i + 1) (sum + max) (maxValue (i+1) k')
					else
						if(newElement <= max) then 
							loop u' k' (i + 1) (sum + max) (max)
						else	
							loop u' k' (i + 1) (sum + max) newElement
		in loop u k 1 0 (maxValue 1 k)
		
		
------------- ZADANIE 2.1 -------------

-- Rekurencyjna definicja typu Zdanie. Zdaniem może być albo pojedyncza zmienna (Z Char), albo dowolna
-- struktura drzewiasta powstała z grupowania pojedynczych zmiennych, jak również całych ich grup.

data Zdanie = Z Char | N Zdanie |
			  K Zdanie Zdanie | A Zdanie Zdanie | C Zdanie Zdanie

-- Funkcja drukuje zdefiniowaną wyżej strukturę zdania w formacie powszechnie stosowanym w matematyce.
-- Jej działanie jest ściśle rekurencyjne - liściami są pojedyncze zmienne, dla których drukuje się pojedynczy znak,
-- a innymi węzłami operatory logiczne, gdzie drukowane są po dwie zmienne lub grupy zmiennych objęte w nawiasy, itp.
			  
drukuj :: Zdanie -> String
drukuj (N x) = "~" ++ drukuj(x)
drukuj (K x y) = "(" ++ drukuj(x) ++ " & " ++ drukuj(y) ++ ")"
drukuj (A x y) = "(" ++ drukuj(x) ++ " | " ++ drukuj(y) ++ ")"
drukuj (C x y) = "(" ++ drukuj(x) ++ " => " ++ drukuj(y) ++ ")"
drukuj (Z x) = [x]

------------- ZADANIE 2.2 -------------

-- Funkcja zwracająca listę znaków, zawierającą zmienne podanego zdania.
-- Jej działanie oparte jest na filtracji posegregowanego ciągu znaków, uzyskanego za pomocą funkcji drukuj.

wypisz_zmienne :: Zdanie -> [Char]
wypisz_zmienne (x) = filtruj (segreguj x)

-- Funkcja pomocnicza, pozostawiająca w podanej liście znaków jedynie małe litery (tj. zmienne w zdaniu).
-- Wykonywane jest to poprzez podwójne użycie na liście funkcji filter, i porównywanie znaków do kodów ASCII.

filtruj :: [Char] -> [Char]
filtruj (x) = Data.List.filter (\x -> x >= 'a') (Data.List.filter (\x -> x <= 'z') x )

-- Funkcja pomocnicza przetwarza zdanie na listę znaków i sortuje ją, jednocześnie pozostawiając po jednym wystąpieniu danego znaku.
-- Jej działanie polega na prostym złożeniu funkcji drukuj (zdanie na listę znaków), sort (sortowanie) i usun_duplikaty (wycinanie duplikatów).

segreguj :: Zdanie -> [Char]
segreguj (x) = usun_duplikaty (sort (drukuj x))

--Funkcja pomocnicza usuwa z posortowanej listy znaków powtarzające się znaki pozostawiając po jednym wystąpieniu każdego znaku.

usun_duplikaty :: [Char] -> [Char]
usun_duplikaty [] = []
usun_duplikaty x = fpomocnicza x []

fpomocnicza :: [Char] -> [Char] -> [Char]
fpomocnicza [] newList = newList
fpomocnicza ([x]) newList = (newList ++ [x])
fpomocnicza (x:y:ys) newList = if x == y
				then fpomocnicza (y:ys) newList
				else fpomocnicza (y:ys) (newList ++ [x])

------------- ZADANIE 2.3 -------------

-- Funkcja sprawdzająca, czy zdanie jest prawdą dla podanej mapy wartości w formacie [('q',False),('r',True)...].
-- Dla dowolnych operacji (iloczynu, sumy, itd.) sprawdza ona po prostu, czy prawdą są zdania wewnętrzne poddane odpowiednim modyfikacjom (np. negacja),
-- a jak dojdzie do samej zmiennej (Z x), pobiera jej wartość z mapy za pomocą funkcji fromList (znajdującej wartość dla klucza w liście par).

sprawdz :: Zdanie -> [(Char,Bool)] -> Bool
sprawdz (N x) l = not (sprawdz x l)
sprawdz (K x y) l = and [sprawdz x l, sprawdz y l]
sprawdz (A x y) l = or [sprawdz x l, sprawdz y l]
sprawdz (C x y) l = or [sprawdz (N x) l, sprawdz y l]
sprawdz (Z x) l = fromList l ! x

------------- ZADANIE 2.4 -------------

-- Funkcja główna, określająca, czy zdanie jest tautologią. Przyjmuje jeden warunek (zdanie), i zwraca wartość True/False.
-- Działa ona poprzez wywołanie funkcji sprawdz_kolejna dla badanego zdania, z mapą wartości o długości równej ilości zmiennych w zdaniu.

jest_tautologia :: Zdanie -> Bool
jest_tautologia zdanie = sprawdz_kolejna zdanie (generuj_sam_falsz (length (wypisz_zmienne zdanie)))

-- Funkcja pomocnicza, zwracająca listę Boolean długości n, zawierającą same elementy False.
-- Działa ona na zasadnie mapowania wartości False na wszystkie elementy listy wszystkich liczb naturalnych od 1 do n.

generuj_sam_falsz :: Int -> [Bool]
generuj_sam_falsz n = Data.List.map (\x -> False) [1..n]

-- Funkcja pomocnicza, zwracająca odpowiedź na pytanie, czy w liście znajdują się same elementy True.
-- Sprawdza ona po kolei głowy kolejnych pod-list, i zwraca False, jeżeli znajdzie jakikolwiek element niebędący True. Jeżeli przejdzie całą listę, zwraca True.

czy_sama_prawda :: [Bool] -> Bool
czy_sama_prawda [] = True
czy_sama_prawda list = if (head list == True) then czy_sama_prawda (tail list) else False

-- Funkcja pomocnicza, generuje kolejny ciąg bitowy, dodając do podanego jako argument ciągu jeden. Jeżeli dojdzie do przepełnienia, zwraca [].
-- Działanie funkcji polega na zanegowaniu pierwszego elementu listy. Jeżeli był on fałszem (i stał się prawdą), funkcja kończy się, jeżeli nie,
-- natępuje przeniesienie, i przechodzi ona rekurencyjnie do ogona listy. W przypadku wywołania na [] (tzn. koniec listy) zwraca [].

kolejny_ciag :: [Bool] -> [Bool]
kolejny_ciag [] = []
kolejny_ciag list = if (not (czy_sama_prawda list))
						then
							(if (head list == True)
								then ([not (head list)] ++ kolejny_ciag (tail list))
								else ([not (head list)] ++ (tail list)))
						else
							[]

-- Funkcja sprawdza, czy zdanie jest spełnione dla podanej listy wartości, i dla wszystkich kolejnych, tworzonych przez dodawanie 1 aż do przepełnienia.
-- Polega ona na wygenerowaniu funkcją zip poprawnej mapy wartości, sprawdzeniu zdania dla niej, i zwróceniu iloczynu logicznego otrzymanej wartości
-- z wartością zdania dla kolejnego ciągu bitowego. Tym sposobem, jeżeli chociaż dla jednej listy wartości zdanie nie jest prawdą, wiemy, że nie jest tautologią.
-- Zwracanie True przy pustej liście nie ma odwzorowania logicznego, jest jedynie sposobem na zakończenie rekurencji (nie zmieni nic, jeśli wcześniej wystąpił fałsz).
							
sprawdz_kolejna :: Zdanie -> [Bool] -> Bool
sprawdz_kolejna zdanie wartosc = if (not (wartosc == []))
									then (and [(sprawdz zdanie (zip (wypisz_zmienne zdanie) wartosc)), (sprawdz_kolejna zdanie (kolejny_ciag wartosc))])
									else True
