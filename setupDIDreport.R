# Import data -------------------------------------------------------------

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

## Stockholmsenkäten --------------------------------------
df <- read_parquet("../DIDapp/data/2023-05-07_ScoredRev.parquet")
df <- df %>%
  rename(Kommun = DIDkommun)

allaKommuner <- df %>%
  distinct(Kommun) %>%
  pull(Kommun)

df <- df %>%
  filter(Kommun %in% jmfKommun)

# remove data from before 2006 for Södertälje, due to lack of comparisons
df <- df %>%
  filter(!ar < 2006)

# define demographic variables of interest
demogr.vars<-read.csv("../DIDapp/data/SthlmsEnk_demographicVariables.csv")
demogr.vars <- demogr.vars$demogr.vars

# final set of items based on psychometric analyses
itemlabels.final <- read_excel("../DIDapp/data/2023-05-07_allItemInfo.xls") %>%
  select(itemnr,item,Index)

# list of all items included in analyses (even those discarded)
allitems <- read.csv("../DIDapp/data/SthlmsEnk_allitems.csv")

# list of item responses for Psykiska/ psykosomatiska besvär, for use in the "persona" visualization
itemresponses <- read.xlsx("../DIDapp/data/SthlmsEnk_04psfRespCats.xls", sheetName = "04psf")

# create vector with all index names
sthlm.index <- itemlabels.final %>%
  distinct(Index) %>%
  pull()

## Skolinspektionen --------------------------------------------------------

# read data from processed file with Rasch based scores
df.si <- read_parquet("../DIDapp/data/SkolinspAk5Scored_2022-12-20.parquet")
# this data is also based on higher score = higher risk

# some functions are based on the SthlmsEnkät labeling of year as "ar"
# we add a duplicate year variable
df.si <- df.si %>%
  mutate(ar = as.numeric(as.character(År)),
         ar = vec_cast(ar, double()))
#
# # read item info
si.items <- read_csv("../DIDapp/data/SkolinspFinalItems_2022-12-20.csv")
# note that all SI items have merged the top 3 response categories (top = highest risk)

# Cutoff values SthlmsEnk -------------------------------------------------------------

# percentiles based on 2006-2020 data for all of Stockholm Stad (~ 110k responses)
# each year's 70th and 90th percentile value was used to calculate an average (not weighted in any way)
# see script "file 04 Distributions and risk limits.R" in https://github.com/pgmj/sthlmsenk/tree/main/OtherScripts
#rslimits <- read.csv("../../DIDapp/data/SthlmsEnk_rslimitsNoRev2022-12-06.csv")
rslimits <- read_csv("../DIDapp/data/2023-05-07_rslimitsNoRev.csv")

# read cutoffs for protective factors
#rslimits.prot <- read_csv("data/2022-12-16_protective.csv")
rslimits.prot <- read_csv("../DIDapp/data/2023-05-07_protective.csv")

rslimits <- cbind(rslimits,rslimits.prot)
rslimits <- rslimits %>%
  relocate(SkolaPositiv, .after = SkolaNegativ)

# for Skolinspektionen ÅK5
rslimits.si <- read_csv("../DIDapp/data/2022-12-20_SkolinspLimits.csv")
rslimits$`Positiv skolanknytning åk 5` <- rslimits.si$`Positiv skolanknytning åk 5`

# vector of years to be included in year selection inputs
årtal <- c(2006,2008,2010,2012,2014,2016,2018,2020,2022)

# vector of risk and protective factors
rsfaktorer <- c('Utagerande','Närsamhälle','Föräldraskap','Psykiska/ psykosomatiska besvär','Vantrivsel i skolan','Positiv skolanknytning','Välbefinnande')

#---- sums.index -> key metrics for each year and municipality----

# create function to calculate metrics for each index
RSsmf <- function(df, i, j) { # input df, index, and index number
  # j <- match(qc({{i}}),sthlm.index)
  df %>%
    group_by(ar, Kommun) %>%
    reframe(
      Medel = mean({{ i }}, na.rm = T), # calculate averages, etc
      StDev = sd({{ i }}, na.rm = T),
      n = n(),
      StErr = StDev / sqrt(n),
      sd.lo = Medel - StDev,
      sd.hi = Medel + StDev,
      n.90 = length(which({{ i }} > rslimits[2, j])) / n * 100,
      n.75 = length(which({{ i }} > rslimits[1, j])) / n * 100
    ) %>%
    rename(År = ar) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    as.data.frame()
}

sums.Utagerande <- RSsmf(df, Utagerande, 1) %>%
  add_column(Faktor = 'Utagerande')
sums.SkolaNegativ <- RSsmf(df, SkolaNegativ, 2) %>%
  add_column(Faktor = 'SkolaNegativ')
sums.SkolaPositiv <- RSsmf(df, SkolaPositiv, 3) %>%
  add_column(Faktor = 'SkolaPositiv')
sums.PsykSomBesv <- RSsmf(df, PsykSomBesv, 4) %>%
  add_column(Faktor = 'PsykSomBesv')
sums.Parenting <- RSsmf(df, Parenting, 5) %>%
  add_column(Faktor = 'Parenting')
sums.Community <- RSsmf(df, Community, 6) %>%
  add_column(Faktor = 'Community')
sums.Wellbeing <- RSsmf(df, Wellbeing, 7) %>%
  add_column(Faktor = 'Wellbeing')

# bind all metrics together in the same dataframe
sums.index <- rbind(sums.Utagerande,
                    sums.SkolaPositiv,
                    sums.SkolaNegativ,
                    sums.PsykSomBesv,
                    sums.Parenting,
                    sums.Community,
                    sums.Wellbeing)

# same but for Skolinspektionens data
sums.Skolinsp <- RSsmf(df.si,Indexvärde, 8) %>%
  add_column(Faktor = 'SkolaPositivSI')

## get key values divided by gender----

RSsmfGender <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>%
    group_by(ar,Kommun,Kön) %>%
    reframe(Medel = mean({{i}}, na.rm = T),
            StDev = sd({{i}},  na.rm = T),
            n = n(),
            StErr = StDev/sqrt(n),
            sd.lo = Medel-StDev,
            sd.hi = Medel+StDev,
            n.90 = length(which({{i}} > rslimits[2,j]))/n*100,
            n.75 = length(which({{i}} > rslimits[1,j]))/n*100) %>%
    rename(År = ar) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    as.data.frame()
}

sums.Utagerande <- RSsmfGender(df, Utagerande, 1) %>%
  add_column(Faktor = 'Utagerande') %>%
  filter(!Kön == '<NA>')
sums.SkolaPositiv <- RSsmfGender(df, SkolaPositiv, 2) %>%
  add_column(Faktor = 'SkolaPositiv') %>%
  filter(!Kön == '<NA>')
sums.SkolaNegativ <- RSsmfGender(df, SkolaNegativ, 3) %>%
  add_column(Faktor = 'SkolaNegativ') %>%
  filter(!Kön == '<NA>')
sums.PsykSomBesv <- RSsmfGender(df, PsykSomBesv, 4) %>%
  add_column(Faktor = 'PsykSomBesv') %>%
  filter(!Kön == '<NA>')
sums.Parenting <- RSsmfGender(df, Parenting, 5) %>%
  add_column(Faktor = 'Parenting') %>%
  filter(!Kön == '<NA>')
sums.Community <- RSsmfGender(df, Community, 6) %>%
  add_column(Faktor = 'Community') %>%
  filter(!Kön == '<NA>')
sums.Wellbeing <- RSsmfGender(df, Wellbeing, 7) %>%
  add_column(Faktor = 'Wellbeing') %>%
  filter(!Kön == '<NA>')

sums.indexG <- rbind(sums.Utagerande,
                     sums.SkolaPositiv,
                     sums.SkolaNegativ,
                     sums.PsykSomBesv,
                     sums.Parenting,
                     sums.Community,
                     sums.Wellbeing)

# same but for Skolinspektionens data
sums.SkolinspG <- RSsmfGender(df.si,Indexvärde, 8)%>%
  add_column(Faktor = 'SkolaPositivSI')


## merge sums.index files----

sums.index <- sums.index %>%
  add_column(Kön = "Flickor och pojkar")

sums.indexG <- sums.indexG %>%
  relocate(Kön, .after = "Faktor")

sums.index <- rbind(sums.index, sums.indexG)

# same but for Skolinspektionens data
sums.si <- sums.Skolinsp %>%
  add_column(Kön = "Flickor och pojkar")
sums.SkolinspG <- sums.SkolinspG %>%
  relocate(Kön, .after = "Faktor")
sums.si <- rbind(sums.si, sums.SkolinspG)
sums.si <- sums.si %>%
  mutate(ar = as.numeric(as.character(År)))


# key values divided by gender and grade ----------------------------------

## get key values divided by gender----

RSsmfGenderG <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>%
    filter(!is.na(ARSKURS)) %>%
    group_by(ar,Kommun,Kön,ARSKURS) %>%
    reframe(Medel = mean({{i}}, na.rm = T),
            StDev = sd({{i}},  na.rm = T),
            n = n(),
            StErr = StDev/sqrt(n),
            sd.lo = Medel-StDev,
            sd.hi = Medel+StDev,
            n.90 = length(which({{i}} > rslimits[2,j]))/n*100,
            n.75 = length(which({{i}} > rslimits[1,j]))/n*100) %>%
    rename(År = ar,
           Årskurs = ARSKURS) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    as.data.frame()
}

sums.Utagerande <- RSsmfGenderG(df, Utagerande, 1) %>%
  add_column(Faktor = 'Utagerande') %>%
  filter(!Kön == '<NA>')
sums.SkolaPositiv <- RSsmfGenderG(df, SkolaPositiv, 2) %>%
  add_column(Faktor = 'SkolaPositiv') %>%
  filter(!Kön == '<NA>')
sums.SkolaNegativ <- RSsmfGenderG(df, SkolaNegativ, 3) %>%
  add_column(Faktor = 'SkolaNegativ') %>%
  filter(!Kön == '<NA>')
sums.PsykSomBesv <- RSsmfGenderG(df, PsykSomBesv, 4) %>%
  add_column(Faktor = 'PsykSomBesv') %>%
  filter(!Kön == '<NA>')
sums.Parenting <- RSsmfGenderG(df, Parenting, 5) %>%
  add_column(Faktor = 'Parenting') %>%
  filter(!Kön == '<NA>')
sums.Community <- RSsmfGenderG(df, Community, 6) %>%
  add_column(Faktor = 'Community') %>%
  filter(!Kön == '<NA>')
sums.Wellbeing <- RSsmfGenderG(df, Wellbeing, 7) %>%
  add_column(Faktor = 'Wellbeing') %>%
  filter(!Kön == '<NA>')

sums.indexGG <- rbind(sums.Utagerande,
                     sums.SkolaPositiv,
                     sums.SkolaNegativ,
                     sums.PsykSomBesv,
                     sums.Parenting,
                     sums.Community,
                     sums.Wellbeing)


# Create risk-level variable -------------------------------------------------

RSrisklevel <- function(df, i) { # input df, index, and index number
  df |>
    mutate(
      !!paste0("Risk",i) := case_when(
        .data[[i]] < rslimits |> select(all_of(i)) |> slice(1) |> pull() ~ "Låg risk",
        .data[[i]] >= rslimits |> select(all_of(i)) |> slice(1) |> pull()
        & .data[[i]] < rslimits |> select(all_of(i)) |> slice(2) |> pull() ~ "Något förhöjd risk",
        .data[[i]] >= rslimits |> select(all_of(i)) |> slice(2) |> pull() ~ "Förhöjd risk")
    ) |>
    select(!!paste0("Risk",i))
}

# Rename RS-factors -------------------------------------------------------

sums.index$Faktor <- car::recode(sums.index$Faktor,"'Community'='Närsamhälle';
                                 'Parenting'='Föräldraskap';
                                 'PsykSomBesv'='Psykiska/ psykosomatiska besvär';
                                 'SkolaNegativ'='Vantrivsel i skolan';
                                 'Wellbeing'='Välbefinnande';
                                 'SkolaPositiv'='Positiv skolanknytning'")

sums.indexGG$Faktor <- car::recode(sums.indexGG$Faktor,"'Community'='Närsamhälle';
                                 'Parenting'='Föräldraskap';
                                 'PsykSomBesv'='Psykiska/ psykosomatiska besvär';
                                 'SkolaNegativ'='Vantrivsel i skolan';
                                 'Wellbeing'='Välbefinnande';
                                 'SkolaPositiv'='Positiv skolanknytning'")


df <- df %>%
  rename(Närsamhälle = Community,
         Föräldraskap = Parenting,
         'Psykiska/ psykosomatiska besvär' = PsykSomBesv,
         'Vantrivsel i skolan' = SkolaNegativ,
         Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )

rslimits <- rslimits %>%
  rename(Närsamhälle = Community,
         Föräldraskap = Parenting,
         'Psykiska/ psykosomatiska besvär' = PsykSomBesv,
         'Vantrivsel i skolan' = SkolaNegativ,
         Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )

#rslimits.prot <- read_csv("../../DIDapp/data/2022-12-16_protective.csv")
rslimits.prot <- read_csv("../DIDapp/data/2023-01-17_rslimitsProt.csv")

rslimits.prot <- rslimits.prot %>%
  rename(Välbefinnande = Wellbeing,
         'Positiv skolanknytning' = SkolaPositiv
  )


# Create long format risklevel df ------------------------
#for coord_polar plot. Might be able to replace others
riskCalc <- function(df,index){
  df %>%
    mutate(riskLevel = case_when(
      .data[[index]] < rslimits |> select(all_of(index)) |> slice(1) |> pull() ~ "Låg risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(1) |> pull() &
        .data[[index]] < rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Något förhöjd risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Förhöjd risk")
    ) %>%
    group_by(Kommun,ar)  %>%
    count(riskLevel) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    mutate(riskLevel = factor(riskLevel, levels = c('Förhöjd risk','Något förhöjd risk','Låg risk','NA'))) %>%
    ungroup() %>%
    add_column(Index = as.character(index)) %>%
    mutate(År = as.factor(ar)) %>%
    select(!all_of(c("n","ar")))
}

df.risk <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in rsfaktorer) {
  df.r1 <- as.data.frame(riskCalc(df, i))
  df.risk <- rbind(df.risk, df.r1)
}

riskCalcGender <- function(df,index){
  df %>%
    mutate(riskLevel = case_when(
      .data[[index]] < rslimits |> select(all_of(index)) |> slice(1) |> pull() ~ "Låg risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(1) |> pull() &
        .data[[index]] < rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Något förhöjd risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Förhöjd risk")
    ) %>%
    group_by(Kommun,ar,Kön)  %>%
    count(riskLevel) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 2)) %>%
    mutate(riskLevel = factor(riskLevel, levels = c('Förhöjd risk','Något förhöjd risk','Låg risk','NA'))) %>%
    ungroup() %>%
    add_column(Index = as.character(index)) %>%
    mutate(År = as.factor(ar)) %>%
    filter(!Kön == "<NA>") %>%
    select(!all_of(c("n","ar")))
}

df.risk.gender <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in rsfaktorer) {
  df.r1 <- as.data.frame(riskCalcGender(df, i))
  df.risk.gender <- rbind(df.risk.gender, df.r1)
}

riskCalcGG <- function(df,index){
  df %>%
    mutate(riskLevel = case_when(
      .data[[index]] < rslimits |> select(all_of(index)) |> slice(1) |> pull() ~ "Låg risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(1) |> pull() &
        .data[[index]] < rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Något förhöjd risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Förhöjd risk")
    ) %>%
    group_by(Kommun,ar,Kön,ARSKURS)  %>%
    count(riskLevel) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 2)) %>%
    mutate(riskLevel = factor(riskLevel, levels = c('Förhöjd risk','Något förhöjd risk','Låg risk','NA'))) %>%
    ungroup() %>%
    add_column(Index = as.character(index)) %>%
    mutate(År = as.factor(ar)) %>%
    rename(Årskurs = ARSKURS) %>%
    filter(!Kön == "<NA>") %>%
    select(!all_of(c("n","ar")))
}

df.risk.gender.arskurs <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in rsfaktorer) {
  df.r1 <- as.data.frame(riskCalcGG(df, i))
  df.risk.gender.arskurs <- rbind(df.risk.gender.arskurs, df.r1)
}

# rename demographic variables for use in selectInput() later
df <- df %>%
  mutate(F5 = factor(F5, levels = c("Mindre än 5 år","5-9 år", "10 år eller mer", "Hela mitt liv"))) %>%
  rename(`Hur länge har du bott i Sverige?` = F5,
         `Vilken högsta utbildning har din mamma/pappa?` = f6ab,
         `Vad bor du i för typ av bostad?` = F7)



# Mobbning ----------------------------------------------------------------

df <- df %>%
  mutate(mobbad = case_when(f60b == 1 ~ "Jag har blivit hånad, förlöjligad, kallad öknamn eller blivit retad på ett obehagligt och sårande sätt",
                            f60c == 1 ~ "Jag har blivit utfrusen av andra elever",
                            f60d == 1 ~ "Jag har blivit slagen, sparkad, knuffad eller stängd inne",
                            f60e == 1 ~ "Någon elev har spritt lögner eller falska rykten om mig och försökt få andra att tycka illa om mig",
                            f60f == 1 ~ "Jag har blivit fråntagen pengar eller saker eller fått saker förstörda
",
                            f60g == 1 ~ "Jag har blivit hotad eller tvingad att göra saker som jag inte ville göra
",
                            f60h == 1 ~ "Lärare har psykat eller på annat sätt varit elaka mot mig
",
                            f60i == 1 ~ "Jag har mobbats på annat sätt.
",
                            F63 == 1 ~ "Har du blivit mobbad eller trakasserad via internet eller SMS/MMS det här läsåret?",
                            f60a == 1 ~ "Nej",
                            TRUE ~ NA
  )) %>%
  mutate(mobbad = factor(mobbad))

# KOLADA new --------------------------------------------------------------

KOLADA <- read_parquet("KOLADA/2023-03-28_KOLADA_data_ready.parquet")

kpi_mean <- KOLADA %>%
  group_by(KPI, kpi, År) %>%
  summarise_at(vars(Andel), list(Andel = mean)) %>%
  add_column(Kommun = "Medel riket", .before = "KPI") %>%
  add_column(Kön = "Alla")

KOLADA <- rbind(KOLADA, kpi_mean)


# RSfigurerRapport --------------------------------------------------------

## For sankey diagrams
lst.data <- read_excel("../DIDapp/data/RISE LST RS-faktorer tabeller OSF.xlsx")
rskontext <- c("Individ","Familj","Kamrater och fritid","Skola","Samhälle")



