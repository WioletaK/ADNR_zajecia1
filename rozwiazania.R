#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

podzielna<-function(){
  pyt <- "Podaj dwie liczby (po przecinku) do sprawdzenia, czy dziela sie jedna przez druga:"
  odp <- as.numeric( strsplit( readline(pyt),",")[[1]] )
  
  if(odp[1]%%odp[2] == 0){
    print(paste("Liczba",odp[1],"jest podzielna przez",odp[2],sep=" "))
  }else{
    print(paste("Liczba",odp[1],"nie dzieli sie bez reszty przez",odp[2],sep=" "))
  }
}

