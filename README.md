PISAvis
=======


[![Coverage Status](https://coveralls.io/repos/MarcinKosinski/PISAvis/badge.svg)](https://coveralls.io/r/MarcinKosinski/PISAvis)

Authors:

- Marcin Kosiński kosinskim@student.mini.pw.edu.pl
- Norbert Ryciak ryciakn@student.mini.pw.edu.pl
- Marta Sommer sommerm@student.mini.pw.edu.pl


<h4> Gotowe dane można wczytać do R jak poniżej </h4>
````{Ruby}
con <- url("http://beta.icm.edu.pl/PISAcontest/data/student2012.rda")
load(con)
````

>
> Można jednak sprobować pobrać dane samemu bezpośrednio ze strony badania
>

<h3> WCZYTANIE PISA2012 DO R </h3>
Poniżej mała instrukcja jak dokopać się do danych z PISA2012, aby działały w **R**.
Dane w formacie `.txt` pobieramy [stąd](http://pisa2012.acer.edu.au/downloads/INT_STU12_DEC03.zip). Następnie w systemie **SAS** tworzymy nowy program, którego 3 pierwsze linie można (ale nie trzeba) wpisać jak poniżej:
```{Ruby}
libname  MD "D:\PISA 2012"; 
filename STU "D:\PISA 2012\INT_STU12_DEC03.txt"; 
options nofmterr;
```
Kolejne linie w programie powinny byc przekopiowane [z tego pliku](http://pisa2012.acer.edu.au/downloads/INT_STU12_SAS.sas). W tym momencie można już wywołać cały program w **SAS**, aby uzyskać pełną bazę danych **PISA2012**. Ponieważ baza zajmuje około 1,5 GB, ograniczymy się jedynie do danych dotyczących Polski, dzięki czemu program **R** będzia działał sprawniej na mniej pojemnym pliku. Posłuży do tego zapytanie **SQL**, które prezentuję poniżej:
```{Ruby}
proc sql;
create table POL as
select *
from Md.Stu
where CNT = 'POL'
;
```
Pomimo, że pierwsza kolumna bazy, z której wybieramy jedynie Polskę, ma widniejący podpis `Country code 3-character`, to jednak po wyświetleniu atrytbutów kolumny widać, że jej nazwa to `CNT`, a `Country code 3-character` to jedynie etykieta. Dodatkowo można w ten sposób odczytać informację o długości znaków w tej kolumnie, która wynosi 3, dlatego ostatecznie w zapytaniu **SQL** widnieje linia `where CNT = 'POL'`. 
Tak pomniejszoną bazę danych eksportuję do formatu `.csv` (możliwe, że bezmyślnie), dzięki procedurze `export`. Wszystkie dotychczasowe komendy i operacja odbywały się w systemie **SAS**.
```{Ruby}
proc export data=Pol
   outfile='D:\PISA 2012\polska.csv'
   dbms=csv
   replace;
run;
```
Ostatecznie z pliku `.csv` można już "tradycyjnie" wczytać dane do pakietu **R**, używając prostego polecenia **read.csv**.
```{Ruby}
POL <- read.csv("D:/PISA 2012/polska.csv", sep=",", h=TRUE)
```

Opisy poszczególnych kolumn można znaleźć w [Codebook'u](http://pisa2012.acer.edu.au/downloads/M_stu_codebook.pdf). Należy pamiętać, że powyższa baza danych dotyczyła jedynie kwestionariuszy wypełnianych przez uczniów.
Więcej na ten temat można znaleźć na stronie [PISA2012](http://pisa2012.acer.edu.au/downloads.php).



<h3>Wyniki uzyskane przez uczniów w Polsce</h3>
Podobne kroki wykonuję się, aby wgrać do pakietu **R** wyniki uzyskane przez Polskich szesnatolatków. Dane w formacie `.txt` pobieramy [stąd](http://pisa2012.acer.edu.au/downloads/INT_COG12_S_DEC03.zip). Przy użyciu tych samych komend w **SAS**, tworzę plik o rozszerzeniu `.csv` zawierający wyniki. Następnie wgrywam je do **R** i łączę z poprzednią ramką danych (być może bezmyślnie).
```{Ruby}
Wyn <- read.csv("D:/PISA 2012/wyniki.csv", sep=",", h=TRUE)

polo <- merge(POL, Wyn)
```
Następnie, by można było ewentualnie przetransportować połączone dane, używam poniższych komend do zapisu scalonej bazy danych w formacie `.txt` i `.csv`
```{Ruby}
write.csv(polo, "D:/PISA 2012/polaczone.csv")

write.table(polo, "D:/PISA 2012/polaczone.txt", sep="\t")
```
Obecnie dane można wczytać poleceniami:
```{Ruby}
ponowne <- read.table("D:/PISA 2012/polaczone.txt", sep="\t", header=TRUE)
ponowne2 <- read.csv("D:/PISA 2012/polaczone.csv", sep=",", h=TRUE)
```
Zbiór danych wczytanych z pliku `.csv` zawiera na początku jedną dodatkową kolumnę zawierającą liczbę porządkową danego gimnazjalisty.





Więcej o PISA2012 można obejrzeć na Konferencji Umiejętności 15-latków: PISA 2012

<a href="http://www.youtube.com/watch?feature=player_embedded&v=FbYxl1_RkTI
" target="_blank"><img src="http://img.youtube.com/vi/FbYxl1_RkTI/1.jpg" 
alt="IMAGE ALT TEXT HERE" width="240" height="180" border="10" /></a>
