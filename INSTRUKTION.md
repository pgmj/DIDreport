# Import och bearbetning av data från Stockholmsenkäten

I mappen `Sthlmsenk` finns filer som används för att ta de olika stegen från en rådatafil till en datafil som är bearbetad för att skapa en DID-rapport.

## 01b Import data.R

Här finns kod för att importera och samordna datastruktur för de olika kommuner i länet som delat data från sin Stockholmsenkät. Funktionen `janitor::compared_df_cols()` hade kunnat underlätta detta arbete, och rekommenderas för den som ska lägga in mera data senare.

Datafilerna sparas under en åtkomstbegränsad mapp internt på RISE, och kan inte delas öppet utan medgivande från respektive kommun. Sökvägen till mappen definieras i variabeln `datafolder`, och kan behöva anpassas beroende på vem som ska importera data och vilket operativsystem som används. Nuvarande kod utgår från hur MacOS hanterar sökvägar.

Det finns ingen anledning att importera ALLA data igen om målsättningen enbart är att lägga till data för en ny kommun eller för ett nytt årtal. Det som då är viktigt är att se till att variabelnamn och ordningen på variablerna stämmer överens med de som redan finns i den sammanslagna datafilen. `Sthlmsenk_variabler.csv` innehåller en tom dataframe med variabelnamn och ordning, och kan användas för att jämföra med den nya datafilen.

## 02 och 02b

I den första filen omkodas alla variabler från svarskategorier till siffror.

I 02b omkodas de variabler som utifrån de psykometriska analyserna visat på behov av omkodning. Om man vill kunna ha kvar de ursprungliga svarskategorierna rekommenderas att en kopia av originalvariabeln görs innan 02b körs.

## 03 estimering av mätvärden

I denna fil estimeras mätvärden för de index/områden som uppvisat adekvat mätkvalitet i de psykometriska analyserna. Detta görs med en loop som går igenom alla index, vilket kan ta lång tid beroende på hur många respondenter som ingår i datafilen. Exempelvis tar 100 000 elever drygt 1h att estimera på en Mac med M1-processor som använder 8 CPU'er.

Du behöver installera paketet RISEkbmRasch från GitHub för att kunna köra koden.

``` r
install.packages('devtools')
devtools::install_github("pgmj/RISEkbmRasch", dependencies = TRUE)
```

OBS att denna kodrad (67 i skrivande stund) behöver anpassas om din dator har färre CPU'er.
``` r
  thetas <- as.data.frame(RIestThetas2(df.if, itemParams = itemParams[[x]], cpu = 8))
```

Du kan kolla hur många CPU'er din dator har genom att köra `parallel::detectCores()`. Det rekommenderas att inte använda alla CPU'er, utan lämna minst en ledig för andra processer.

När estimeringen är klar sparar du datafilen som en .parquet-fil.

## 04 risknivåer

Denna behöver inte köras om det inte finns anledning att ta fram nya gränsvärden för gruppindelningen av risknivåer.

Det finns även en fil som heter `rslimits.qmd` som mera noggrannt går igenom hur gränsvärdena för risknivåer tas fram.

