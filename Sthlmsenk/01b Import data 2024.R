
# First, read data for the municipality in focus, and adjust available columns/variables
# as needed (a vector of necessary variables should be made). Then jump to the
# General recode preprocessing part and run the rest of the code. After that, open script 03
# to estimate person locations for each index

library(foreign)
#library(haven)
library(tidyverse)
library(arrow)
library(readxl)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename
filter <- dplyr::filter

datafolder <- "~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/"

# Import data -------------------------------------------------------------

itemParams <- readRDS("Sthlmsenk/itemParams.Rdata")

# make vector of demographic variables to include in final datafile
demogr.vars <- read.csv("Sthlmsenk/demographicVariables.csv")
demogr.vars <- demogr.vars$demogr.vars

# all analyzed items
allAnalyzedItems <- read.csv("Sthlmsenk/allitems.csv")
allAnalyzedItemsADD <- data.frame(itemnr = c("F62","F64","F69"),
                                  item = c("Har du varit med om att mobba eller trakassera andra elever i skolan det här läsåret?",
                                           "Har du varit med om att mobba eller trakassera andra elever via internet eller SMS/MMS det här läsåret?",
                                           "Brukar du vara på fritidsgård eller 'träffpunkt'?"),
                                  Index = c(NA,NA,NA))
allAnalyzedItems <- rbind(allAnalyzedItems,allAnalyzedItemsADD)
##### NOTE:
### when comparing datafiles in order to enable binding them together, the function
### janitor::compared_df_cols() will be very useful and is not (yet) used in the code below

### This is a lookup table for the variable names used in our analyses
recode_map <- read_csv("Sthlmsenk/recmap_sorted.csv")
# reference variables in order
ref_vars <- read_csv("Sthlmsenk/ref_vars.csv")

## Stockholm stad ----------------------------------------------------------

# read and combine data
df.1420 <- read.spss(paste0(datafolder,"Stockholm Stad/SE 2014-2020 KI Leifman.sav"),
                            to.data.frame = TRUE)
df.0612 <- read.spss(paste0(datafolder,"Stockholm Stad/2006-2012 Stockholmsenkäten 201126.sav"),
                     to.data.frame = TRUE)

df.sthlm <- rbind(df.0612,df.1420)

# subset selected demographic variables and items
df.sthlm <- df.sthlm %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

# to match Vaxholms variables
df.sthlm <- df.sthlm %>%
  add_column(SkolID_gammal = NA, .before = "SkolSDO") %>%
  add_column(DIDkommun = 'Stockholm')


### 2024 --------------------------------------------------------------------

## temp test för stadsdelarna
# df.sthlm <- read.spss(paste0(datafolder,"Stockholm Stad/2024/Stockholmsenkäten 2002-2024 Stockholm.sav"),
#           to.data.frame = TRUE)
#
# df2 <- df.sthlm %>%
#   rename(F14 = F14a,
#          FNY12020 = F14b,
#          F18 = F18a) %>%
#   select(all_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolSDO"))) %>%
#   add_column(SkolID_gammal = NA, .before = "SkolSDO") %>%
#   add_column(DIDkommun = 'Stockholm') %>%
#   mutate(ar = str_squish(ar),
#          Skolnamn = str_squish(Skolnamn)) %>%
#   mutate(ar = as.numeric(ar)) %>%
#   filter(ar > 2005)
# df <- df2

# Sthlm 2022 och 2024
s2224 <- read.spss(paste0(datafolder,"Stockholm Stad/2024/Stockholmsenkäten 2002-2024 Stockholm.sav"),
                   to.data.frame = TRUE)

s2224f <-
  s2224 %>%
  mutate(ar = str_squish(ar)) %>%
  filter(ar %in% c("2022","2024"))

s2224f <-
  s2224f %>%
  rename(F14 = F14a,
         FNY12020 = F14b) %>%
  mutate(across(c(F18a,F18b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = F))) %>%
  #mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F18 = pmax(F18a,F18b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F18 = recode(F18,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

s2224f <-
  s2224f %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(SkolID_gammal = NA, .before = "SkolSDO") %>%
  add_column(DIDkommun = 'Stockholm')

df.sthlm <- rbind(df.sthlm,s2224f)

# reordering variables with arrange to simplify dealing with Origos change in variable names from 2022-2024.
#ref_vars <- data.frame(ref = names(df.sthlm)) %>%
#  arrange(ref)

#write_csv(ref_vars,"Sthlmsenk/ref_vars.csv")

#setdiff(recode_map$itemnr_old,ref_vars$ref)

df.sthlm <- df.sthlm %>%
  select(all_of(ref_vars$ref))

## Vallentuna ----------------------------------------------------------
df.vtuna1618 <- read.spss(paste0(datafolder,"Vallentuna/Sthlmsenk/Stockholmsenkäten 2018 Vallentuna 2016-2018.sav"),
                          to.data.frame = TRUE)
df.vtuna20 <- read.spss(paste0(datafolder,"Vallentuna/Sthlmsenk/Stockholmsenkäten 2020 Vallentuna.sav"),
                        to.data.frame = TRUE)

df.vtuna1618 <- df.vtuna1618 %>%
  rename(ARSKURS = Arskurs)

df.vtuna20 <- df.vtuna20 %>%
  rename(Kön = F2)

df.vtuna1618 <- df.vtuna1618 %>%
  rename(Skolnamn = SkolID) %>%
  add_column(Skolenhetskod = NA)

## subset variables
df.vtuna1 <- df.vtuna1618 %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))
df.vtuna2 <- df.vtuna20 %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

# Columns `F67`, and `F68` don't exist.
df.vtuna1 <- df.vtuna1 %>%
  add_column(F67 = NA, .after = "f101l") %>%
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>%
  add_column(FNY12020 = NA, .after = "F14") %>%
  add_column(F41= NA, .after = "F34") %>%
  add_column(F44 = NA, .before = "F51") %>%
  add_column(F37 = NA, .after = "F20") %>%
  add_column(F45 = NA, .after = "F35") %>%
  add_column(FNY22020 = NA, .after = "F40")

# Then Vallentuna 2022
df.vtuna22 <- read.spss(paste0(datafolder,"Vallentuna/Sthlmsenk/Stockholmsenkäten 2022 Vallentuna.sav"),
                        to.data.frame = TRUE) %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

#setdiff(names(df.vtuna1),names(df.vtuna22))

df.vtuna <- rbind(df.vtuna1,df.vtuna2,df.vtuna22)

# to match Vaxholms variables
df.vtuna <- df.vtuna %>%
  add_column(SkolID_gammal = NA, SkolSDO = NA) %>%
  add_column(DIDkommun = 'Vallentuna')

# TEST reorder vars

df.vtuna <- df.vtuna %>%
  select(all_of(ref_vars$ref))

### 2024 ----------------------
df.vtuna24 <- read.spss(paste0(datafolder,"Vallentuna/Sthlmsenk/Stockholmsenkäten 2024 Vallentuna (1).sav"),
                                                 to.data.frame = TRUE)

### before renaming the new variable names to the old ones to enable comparisons, we need to deal with some new items
# F19a Snusar du så kallat vitt snus/nikotinpåse (tobaksfritt snus med nikotin)?
# F19b Snusar du snus med tobak?
# we want the "higher" response from either item
df.vtuna24 <- df.vtuna24 %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.vtuna24r <- df.vtuna24 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Vallentuna') %>%
  select(all_of(recode_map$itemnr_new))

names(df.vtuna24r) <- recode_map$itemnr_old

#names(df.vtuna24) <- names(df.vtuna)
#janitor::compare_df_cols(df.vtuna24r,df.vtuna)
df.vtuna <- rbind(df.vtuna,df.vtuna24r)
#df <- df.vtuna
#write_parquet(df.vtuna24,paste0(datafolder,"DID_klart/2024-10-08_DataPreRecode_Vallentuna2024.parquet"))

#df <- df.vtuna24

## Vaxholm ----------------------------------------------------------

df.vaxholm <- read.spss(paste0(datafolder,"Vaxholm/Sthlmsenk/Stockholmsenkäten 2008-2022 Vaxholm (1).sav"),
                        to.data.frame = TRUE)

df.vaxholm <- df.vaxholm %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Vaxholm')

df.vaxholm <- df.vaxholm %>%
  select(all_of(ref_vars$ref))

## Danderyd ----------------------------------------------------------------

df.danderyd <- read.spss(paste0(datafolder,"Danderyd/Stockholmsenkäten 2014-2022 Danderyd.sav"),
                         to.data.frame = TRUE)

df.danderyd <- df.danderyd %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Danderyd')

df.danderyd <- df.danderyd %>%
  select(all_of(ref_vars$ref))

## Täby --------------------------------------------------------------------

df.täby <- read.spss(paste0(datafolder,"Täby/Stockholmsenkäten 2016 & 2020-2022 Täby (2).sav"),
                     to.data.frame = TRUE)

df.täby <- df.täby %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Täby')

df.täby <- df.täby %>%
  select(all_of(ref_vars$ref))

## Södertälje --------------------------------------------------------------

df.södertälje <- read.spss(paste0(datafolder,"Södertälje/Stockholmsenkäten 2002-2022 Södertälje_granskad.sav"),
                           to.data.frame = TRUE)
# estimerade <- names(df) %>% tail(7)
df.södertälje <- df.södertälje %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(Skolenhetskod = NA, .after = "Skolkommun") %>%
  add_column(Skolnamn = NA, .before = "ARSKURS") %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Södertälje')

df.södertälje <- df.södertälje %>%
  select(all_of(ref_vars$ref))


## Sigtuna -----------------------------------------------------------------

df.sigtuna <- read.spss(paste0(datafolder,"Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (9).sav"),
                        to.data.frame = TRUE)
#df.sigtuna2 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (4).sav"))

df.sigtuna <- df.sigtuna %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Sigtuna')

df.sigtuna <- df.sigtuna %>%
  select(all_of(ref_vars$ref))

### 2024 --------------------------------------------------------------------


sigtuna24 <- read.spss(paste0(datafolder,"Sigtuna/2024/Stockholmsenkäten 2024 Sigtuna.sav"),
                       to.data.frame = TRUE)

### before renaming the new variable names to the old ones to enable comparisons, we need to deal with some new items
# F19a Snusar du så kallat vitt snus/nikotinpåse (tobaksfritt snus med nikotin)?
# F19b Snusar du snus med tobak?
# we want the "higher" response from either item
sigtuna24 <- sigtuna24 %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))


sigtuna24r <- sigtuna24 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Sigtuna') %>%
  select(all_of(recode_map$itemnr_new))

names(sigtuna24r) <- recode_map$itemnr_old

df.sigtuna <- rbind(df.sigtuna,sigtuna24r)

## Järfälla ----------------------------------------------------------------

df.jfl1 <- read.spss(paste0(datafolder,"Järfälla/Stockholmsenkäten 2006-2020 Järfälla.sav"),
                     to.data.frame = TRUE)
df.jfl2 <- read.spss(paste0(datafolder,"Järfälla/Stockholmsenkäten 2022 Järfälla.sav"),
                     to.data.frame = TRUE)

df.jfl1 <- df.jfl1 %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Järfälla') %>%
  select(all_of(ref_vars$ref))

df.jfl2 <- df.jfl2 %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Järfälla') %>%
  select(all_of(ref_vars$ref))


### 2024 --------------------------------------------------------------------

# 2024 (maybe make this section a separate R-file and use source()?)
df.jfl3 <- read.spss(paste0(datafolder,"Järfälla/Sthlmsenkät 2024/Stockholmsenkäten 2024 Järfälla.sav"),
                     to.data.frame = TRUE)

### before renaming the new variable names to the old ones to enable comparisons, we need to deal with some new items
# F19a Snusar du så kallat vitt snus/nikotinpåse (tobaksfritt snus med nikotin)?
# F19b Snusar du snus med tobak?
# we want the "higher" response from either item
df.jfl3 <- df.jfl3 %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                 as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.jfl3r <- df.jfl3 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Järfälla') %>%
  select(all_of(recode_map$itemnr_new))

names(df.jfl3r) <- recode_map$itemnr_old

df.jfl <- rbind(df.jfl1,df.jfl2,df.jfl3r)

## Lidingö -----------------------------------------------------------------
df.lid1 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2016 Lidingö.sav"),
                     to.data.frame = TRUE) %>%
  rename(
    ARSKURS = Arskurs,
    Skolnamn = SkolID
  ) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    Skolenhetskod = NA,
    DIDkommun = "Lidingö"
  ) %>%
  relocate(Skolenhetskod, .after = "Skolkommun") %>%
  add_column(F67 = NA, .after = "f101l") %>%
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>%
  add_column(FNY12020 = NA, .after = "F14") %>%
  add_column(F41 = NA, .after = "F34") %>%
  add_column(F44 = NA, .before = "F51") %>%
  add_column(F37 = NA, .after = "F20") %>%
  add_column(F45 = NA, .after = "F35") %>%
  add_column(FNY22020 = NA, .after = "F40") %>%
  select(all_of(ref_vars$ref))

df.lid2 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2018 Lidingö.sav"),
                     to.data.frame = TRUE) %>%   rename(
    ARSKURS = Arskurs,
    Skolnamn = SkolID
  ) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    Skolenhetskod = NA,
    DIDkommun = "Lidingö"
  ) %>%
  relocate(Skolenhetskod, .after = "Skolkommun") %>%
  add_column(F67 = NA, .after = "f101l") %>%
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>%
  add_column(FNY12020 = NA, .after = "F14") %>%
  add_column(F41 = NA, .after = "F34") %>%
  add_column(F44 = NA, .before = "F51") %>%
  add_column(F37 = NA, .after = "F20") %>%
  add_column(F45 = NA, .after = "F35") %>%
  add_column(FNY22020 = NA, .after = "F40") %>%
  select(all_of(ref_vars$ref))


df.lid3 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2020 Lidingö.sav"),
                     to.data.frame = TRUE) %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  ) %>%
  select(all_of(ref_vars$ref))

df.lid4 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2022 Lidingö.sav"),
                     to.data.frame = TRUE) %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  ) %>%
  select(all_of(ref_vars$ref))

df.lidingö <- rbind(df.lid1,
                    df.lid2,
                    df.lid3,
                    df.lid4)


### 2024 --------------------------------------------------------------------

lid24 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2024 Lidingö.sav"),
          to.data.frame = TRUE)

### before renaming the new variable names to the old ones to enable comparisons, we need to deal with some new items
# F19a Snusar du så kallat vitt snus/nikotinpåse (tobaksfritt snus med nikotin)?
# F19b Snusar du snus med tobak?
# we want the "higher" response from either item
lid24 <- lid24 %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

lid24r <- lid24 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Lidingö') %>%
  select(all_of(recode_map$itemnr_new))

# data.frame(old = recode_map$itemnr_old,
#            new = names(lid24r)) %>%
#   View()

names(lid24r) <- recode_map$itemnr_old

#df <- lid24r

df.lidingö <- rbind(df.lidingö,lid24r)

## Botkyrka ----------------------------------------------------------------

df.botkyrka <- read.spss(paste0(datafolder,"Botkyrka/Stockholmsenkäten 2004-2022 Botkyrka (4).sav"),
                                to.data.frame = TRUE)

df.botkyrka <- df.botkyrka %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Botkyrka") %>%
  select(all_of(ref_vars$ref))


## Haninge -----------------------------------------------------------------

haninge <- read.spss("/Volumes/rise/Gemensam/Framtidens socialtjänst/Data från Haninge/Stockholmsenkäten 2002-2022 Haninge.sav", to.data.frame = TRUE)
df.haninge <- haninge %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Haninge") %>%
  select(all_of(ref_vars$ref))


## Sundbyberg --------------------------------------------------------------

sundbyberg <- read.spss(paste0(datafolder,"Sundbyberg/Sundbyberg Rådata/Stockholmsenkäten 2002-2022 Sundbyberg.sav"), to.data.frame = TRUE)

df.sundbyberg <- sundbyberg %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Sundbyberg") %>%
  select(all_of(ref_vars$ref))


## Värmdö 2024 ------------------------------------------------------------------

varmdo <- read.spss(paste0(datafolder,"Värmdö/Stockholmsenkäten 2014-2024 Värmdö.sav"), to.data.frame = TRUE)
# looks like same issue with variable names as others from 2024
# df.varmdo <- varmdo %>%
#   select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
#   add_column(DIDkommun = "Värmdö")

### before renaming the new variable names to the old ones to enable comparisons, we need to deal with some new items
# F19a Snusar du så kallat vitt snus/nikotinpåse (tobaksfritt snus med nikotin)?
# F19b Snusar du snus med tobak?
# we want the "higher" response from either item
df.varmdo <- varmdo %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.varmdo <- df.varmdo %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Värmdö') %>%
  select(all_of(recode_map$itemnr_new))

names(df.varmdo) <- recode_map$itemnr_old


## Solna 2024 --------------------------------------------------------------

# Sundbyberg och Sollentuna jmf

solna <- read.spss(paste0(datafolder,"Solna/Stockholmsenkäten 2002-2024 Solna.sav"), to.data.frame = TRUE)

solna2 <- solna %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.solna <- solna2 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Solna') %>%
  select(all_of(recode_map$itemnr_new))

names(df.solna) <- recode_map$itemnr_old
#df <- df.solna %>%
#  filter(!ar == 2024)

## Upplands-Bro 2024 -------------------------------------------------------

uppbro <- read.spss(paste0(datafolder,"Upplands-Bro/Stockholmsenkäten 2014-2024 Upplands-Bro.sav"), to.data.frame = TRUE)

uppbro2 <- uppbro %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = T))) %>%
  mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b, na.rm = T)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.uppbro <- uppbro2 %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Upplands-Bro') %>%
  select(all_of(recode_map$itemnr_new))

names(df.uppbro) <- recode_map$itemnr_old

# Combine all data --------------------------------------------------------

df <- rbind(df.sthlm,
            df.vtuna,
            df.vaxholm,
            df.danderyd,
            df.täby,
            df.södertälje,
            df.jfl,
            df.sigtuna,
            df.lidingö,
            df.botkyrka,
            df.haninge,
            df.sundbyberg,
            df.varmdo,
            df.solna,
            df.uppbro)

#write_parquet(df2,paste0(datafolder,"DID_klart/2025-02-13_SthlmStad_DataPreRecode.parquet"))
#write_parquet(df,paste0(datafolder,"DID_klart/2025-03-17_DataPreRecode_Lidingö2024.parquet"))

# create data frame with 0 rows and named variables as a template
#names <- data.frame(matrix(ncol = length(names(df)), nrow = 0))
# provide column names
#colnames(names) <- names(df)
#write_csv(names, "Sthlmsenk/variabler.csv")

## On to next script! 02 for recodings.
