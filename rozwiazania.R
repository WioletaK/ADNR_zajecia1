#1. Napisz funkcję sprawdzająca czy jedna liczba jest podzielna przez druga użyj - %%

podzielnosc<-function(){
  pyt <- "Podaj dwie liczby (po przecinku) do sprawdzenia, czy dziela sie jedna przez druga:"
  odp <- as.numeric( strsplit( readline(pyt),",")[[1]] )
  
  if(odp[2] == 0){
    print("Nie dzielimy przez 0!")
  }else{
    if(odp[1]%%odp[2] == 0){
      print(paste("Liczba",odp[1],"jest podzielna przez",odp[2],sep=" "))
    }
    else{
      print(paste("Liczba",odp[1],"nie dzieli sie bez reszty przez",odp[2],sep=" "))
    }
  }
}


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu?

v1 <- 120
v2 <- 90
#t1 = s/v1
#t2 = s/v2
#v_sr = 2s / (t1+t2) = 2s / (s/v1+s/v2) = 2s / ((s*v2+s*v1)/(v1*v2) = 2s*v1*v2/(s*(v1+v2))
v_sr = 2*v1*v2 / (v1+v2)
print(paste("Srednia predkosc pociagu z Lublina do Warszawy wynosila",v_sr,sep = " "))


#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

#wektory podawane przez uzytkownika
wsp_R_Pearsona<-function(){
  pyt <- "Podaj pierwszy wektor (liczby oddzielone przecinkiem):"
  vec1 <- as.numeric( strsplit( readline(pyt),",")[[1]] )
  pyt <- "Podaj drugi wektor (liczby oddzielone przecinkiem):"
  vec2 <- as.numeric( strsplit( readline(pyt),",")[[1]] )
  v1_sr <- mean(vec1)
  v2_sr <- mean(vec2)
  
  if(length(vec1)!= length(vec2)){
    print("Wektory sa roznej dlugosci")
  }else{
    licznik <- 0
    m1 <- 0
    m2 <- 0
    for(i in 1:length(vec1)){
      licznik <- licznik + (vec1[i]-v1_sr)*(vec2[i]-v2_sr)
      m1 <- m1 + (vec1[i]-v1_sr)^2
      m2 <- m2 + (vec2[i]-v2_sr)^2
    }
    #print(licznik)
    mianownik <- sqrt(m1)*sqrt(m2)
    #print(mianownik)
    wsp <- licznik/mianownik
    print(paste("Wspolczynnik korelacji R Pearsona dla podanych wektorow wynosi:",wsp,sep=" "))

    if(wsp<= -0.5){print("Korelacja miedzy nimi jest silnie ujemna.")}
    else{if(wsp < 0){print("Korelacja miedzy nimi jest slabo ujemna.")}
      else{if(wsp == 0){print("Brak korelacji miedzy wektorami.")}
        else{if(wsp < 0.5){print("Korelacja miedzy nimi jest slabo dodatnia.")}
          else{print("Korelacja miedzy nimi jest silnie dodatnia")}
        }
      }
    }
  }
}

#wektory zaczytane z pliku
wsp_R_Pearsona<-function(vec1,vec2){
  v1_sr <- mean(vec1)
  v2_sr <- mean(vec2)
  
  if(length(vec1)!= length(vec2)){
    print("Wektory sa roznej dlugosci")
  }else{
    licznik <- 0
    m1 <- 0
    m2 <- 0
    for(i in 1:length(vec1)){
      licznik <- licznik + (vec1[i]-v1_sr)*(vec2[i]-v2_sr)
      m1 <- m1 + (vec1[i]-v1_sr)^2
      m2 <- m2 + (vec2[i]-v2_sr)^2
    }
    #print(licznik)
    mianownik <- sqrt(m1)*sqrt(m2)
    #print(mianownik)
    wsp <- licznik/mianownik
    print(paste("Wspolczynnik korelacji R Pearsona dla podanych wektorow wynosi:",wsp,sep=" "))
    
    if(wsp<= -0.5){print("Korelacja miedzy nimi jest silnie ujemna.")}
    else{if(wsp < 0){print("Korelacja miedzy nimi jest slabo ujemna.")}
      else{if(wsp == 0){print("Brak korelacji miedzy wektorami.")}
        else{if(wsp < 0.5){print("Korelacja miedzy nimi jest slabo dodatnia.")}
          else{print("Korelacja miedzy nimi jest silnie dodatnia")}
        }
      }
    }
  }
}
wsp_R_Pearsona(data$wzrost,data$waga)


#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn.
#W kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile.
#ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1).

stworzDataFrame <- function(ile=1){
  if (ile>0){
    if(!is.integer(ile)){
      print("Wprowadzona iczba wierszy zostanie zaokraglona w dol do liczby calkowitej")
    }else{}
    
    print("Podaj nazwy kolumn oddzielone spacja:")
    columns <- readline(prompt = "kolumny: ")
    nazwy_kolumn <- strsplit(columns, " ")
    
    df <- data.frame(matrix(NA, nrow = ile, ncol=lengths(nazwy_kolumn)))
    #names(df) <- t(unlist(nazwy_kolumn))
    names(df) <- nazwy_kolumn[[1]]
    
    for(i in 1:ile){
      komunikat <- paste("Podaj wartosci dla ",i,"-ego wiersza oddzielone spacja: ")
      wartosci <- as.character(strsplit(readline(komunikat), " ")[[1]])
      j = 1
      for(column in colnames(df)){
        df[i, column] <- wartosci[j]
        j = j+1
      }
    }
    print(df)
    View(df)
  }else{print("Liczba wierszy powinna byc >0")}
  
}
stworzDataFrame(3)


#5 Napisz funkcję, która pobiera sciezke Katalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median, min, max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
#UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip. Wykonując obliczenia pomiń brakujące wartości.
#liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ ...}
#Lista plików w katalogu: list.files(sciezka)
#Omijanie na : na.omit(myDataFrame[[nazwaKolumny]])
#Do złączania stringów: paste("string1","string2",sep="TU WSTAW SEPARATOR")
#Gdy mamy rózne oznaczenia NA w plikach możemy wykorzystać (w tym wypadku pusty znak i NA: na.strings=c("","NA")

liczZplikow<-function(sciezka="C:/Users/Wiola/Desktop/Disk_D/Wiola/PJATK/BigData_2020/semestr2/ADNR/cwiczenia/zajecia1/Praca_domowa/ADNR_zajecia1/smogKrakow",
                 nazwaKolumny="3_pressure",
                 jakaFunkcja="mean",
                 dlaIluPlikow=1){
  #sciezka <- readline("Podaj sciezke do katalogu: ")
  
  nazwaKolumny <- paste("X",nazwaKolumny,sep="")
  lista_plikow <- list.files(sciezka)
  i <- 1
  for(plik in lista_plikow){
    if(i <= min(dlaIluPlikow,length(lista_plikow))){
      #print(paste(sciezka,plik,sep="/"))
      dane <- read.csv(file = paste(sciezka,plik,sep="/"),dec=",")
      #print(dim(dane))
      #print(class(dane))
      dane <- na.omit(dane[[nazwaKolumny]],na.strings=c("","NA"))
      #print(class(dane))
      #print(dim(dane))
      #print(dane)
      attributes(dane)$na.action <- NULL
      wynik<-switch(jakaFunkcja,
                    mean = mean(dane),median = median(dane),min = min(dane),max = max(dane)
      )
      print(paste("W pliku",plik,"wartosc funkcji",jakaFunkcja,"na kolumnie",nazwaKolumny,"wynosi",wynik))
      i <- i+1
    }
  }
}
liczZplikow(nazwaKolumny="196_pressure",jakaFunkcja="median",dlaIluPlikow=12)
