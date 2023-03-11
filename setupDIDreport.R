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
    mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
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
# sums.Skolinsp <- RSsmf(df.si,Indexvärde, 8) %>%
#   add_column(Faktor = 'SkolaPositivSI')

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
    mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
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
# sums.SkolinspG <- RSsmfGender(df.si,Indexvärde, 8)%>%
#   add_column(Faktor = 'SkolaPositivSI')


## merge sums.index files----

sums.index <- sums.index %>%
  add_column(Kön = "Flickor och pojkar")

sums.indexG <- sums.indexG %>%
  relocate(Kön, .after = "Faktor")

sums.index <- rbind(sums.index, sums.indexG)

# same but for Skolinspektionens data
# sums.si <- sums.Skolinsp %>%
#   add_column(Kön = "Flickor och pojkar")
# sums.SkolinspG <- sums.SkolinspG %>%
#   relocate(Kön, .after = "Faktor")
# sums.si <- rbind(sums.si, sums.SkolinspG)
# sums.si <- sums.si %>%
#   mutate(ar = as.numeric(as.character(År)))

# Create risk-level variable -------------------------------------------------

RSrisklevel <- function(df, i) { # input df, index, and index number
  df |>
    mutate(
      !!paste0("Risk",i) := case_when(
        .data[[i]] < rslimits |> select(all_of(i)) |> slice(1) |> pull() ~ "Låg risk",
        .data[[i]] >= rslimits |> select(all_of(i)) |> slice(1) |> pull()
        & .data[[i]] < rslimits |> select(all_of(i)) |> slice(2) |> pull() ~ "Medelhög risk",
        .data[[i]] >= rslimits |> select(all_of(i)) |> slice(2) |> pull() ~ "Hög risk")
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
        .data[[index]] < rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Medelhög risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Hög risk")
    ) %>%
    group_by(Kommun,ar)  %>%
    count(riskLevel) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 1)) %>%
    mutate(riskLevel = factor(riskLevel, levels = c('Hög risk','Medelhög risk','Låg risk','NA'))) %>%
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
        .data[[index]] < rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Medelhög risk",
      .data[[index]] >= rslimits |> select(all_of(index)) |> slice(2) |> pull() ~ "Hög risk")
    ) %>%
    group_by(Kommun,ar,Kön)  %>%
    count(riskLevel) %>%
    mutate(Andel = (100 * n / sum(n)) %>% round(digits = 2)) %>%
    mutate(riskLevel = factor(riskLevel, levels = c('Hög risk','Medelhög risk','Låg risk','NA'))) %>%
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
