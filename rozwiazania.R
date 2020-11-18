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