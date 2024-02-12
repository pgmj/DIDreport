# Data i Dialog kommunrapporter

Här finns källkod för att generera automatiserade rapporter eller lägesbilder för kommuner. Arbetet sker inom ramen för projektet [Data i Dialog](https://www.ri.se/sv/vad-vi-gor/projekt/data-i-dialog-risk-och-skyddsfaktorer-for-barn-och-unga) som RISE Research Institutes of Sweden genomför i samverkan med finansiären Länsstyrelsen i Stockholm.

## Struktur

Huvudfilen i detta projekt är `DIDreport3.qmd`, som i sin tur anropar två filer:

- `DIDreportSetup.R` där data importeras och bearbetas
- `DIDreportFunctions.R` där alla funktioner som skapar figurer i rapporten definieras

För att automatisera skapandet av rapportfiler för flera kommuner används två filer:

- `render.R` som läser information från
- `DIDreportParameters.xls` där varje kommun har en rad som visar vilka kommuner de vill jämföra sig med

## Dataunderlag

### Stockholmsenkäten

En stor del av den data som bearbetas och visualiseras är hämtad från skolenkäten Stockholmsenkäten, till stor del baserat på de [psykometriska analyser](https://pgmj.github.io/sthlmsenk/) som RISE genomfört, också finansierat av Länsstyrelsen i Stockholm.

Data ägs av kommunerna och kan därför ej delas öppet här. Hur data har bearbetats innan den används i koden som återfinns här är [dokumenterat i anslutning till de psykometriska analyserna](https://github.com/pgmj/sthlmsenk?tab=readme-ov-file#om-data).

### KOLADA

I mappen `KOLADA` finns filen `KOLADAsetup.R` som dokumenterar hur hämtning av nyckeltal för samtliga kommuner i Stockholms Län görs, hur hämtade data omformateras inför användningen i denna rapport, samt att den färdiga datafilen skrivs till en fil i formatet ".parquet".

Filformatet ".parquet" kan läsas och skrivas med paketet [`library(arrow)`](https://arrow.apache.org/docs/r/) som möjliggör både hög kompression och mycket snabb läs-/skrivhastighet.

### Skolverket

I mappen `Skolverket` finns dels manuellt nedladdade CSV-filer som hämtats från [skolverket.se](https://www.skolverket.se/skolutveckling/statistik/sok-statistik-om-forskola-skola-och-vuxenutbildning) eftersom dessa data inte återfinns i Skolverkets databas. CSV-filerna bearbetas i filen `skolverket.R` och resultatet skrivs till filen `skolverketDemografi.parquet`

Arbetet med att ta fram visualisering av data från Skolverket finns i `skolverketVisualisering.qmd` och har införlivats i `DIDreportFunctions.R` för användning i rapportmallen.

Data har även hämtats via API från Skolverkets öppna databas. Detta arbete har till stor del dokumenterats här: <https://pgmj.github.io/SkolverketAPI/skolverketapi.html>

Filen som hämtar data via API för kommunerna i Stockholms och Uppsalas län är `2023-08-21_loop_over_all_municipalities.R`.



