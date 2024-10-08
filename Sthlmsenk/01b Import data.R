
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
allAnalyzedItemsADD <- data.frame(itemnr = c("F62","F64"),
                                  item = c("Har du varit med om att mobba eller trakassera andra elever i skolan det här läsåret?",
                                           "Har du varit med om att mobba eller trakassera andra elever via internet eller SMS/MMS det här läsåret?"),
                                  Index = c(NA,NA))
allAnalyzedItems <- rbind(allAnalyzedItems,allAnalyzedItemsADD)
##### NOTE:
### when comparing datafiles in order to enable binding them together, the function
### janitor::compared_df_cols() will be very useful and is not (yet) used in the code below

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

# Sthlm 2022 och 2024
s2224 <- read.spss(paste0(datafolder,"Stockholm Stad/2024/Stockholmsenkäten 2002-2024 Stockholm.sav"),
                   to.data.frame = TRUE)
#s2224 %>% count(ar)
#glimpse(s2224)
s2224f <-
  s2224 %>%
  mutate(ar = str_squish(ar)) %>%
  filter(ar %in% c("2022","2024"))

#
# names(df.sthlm)
# names(s2224f)
# # looks like these are missing from the new data: "F14"           "FNY12020"      "F18"
# missing <- c("F14"       ,    "FNY12020"      ,"F18" )
# recode_map <- read_csv("Sthlmsenk/origo2024_recode_map.csv")
# recode_map %>%
#   filter(itemnr_old %in% missing)
# ## F14a och F14b angår cigaretter m tobak och e-cigaretter, d.v.s:
# # F14a = F14, och F14b = FNY12020
# ## F18a och F18b angår vitt snus och snus med tobak (OBS omvänt från F14)
# s2224 %>%
#   count(ar,F14b) %>% # F14b och F18a har varit med sedan 2022
#   as.data.frame()

# rename and recode... then go to Järfälla to do similar work for the new snus variables
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
  mutate(F18 = pmax(F18a,F18b)) %>%
  # recode back to character categories for later recoding to work
  mutate(F18 = recode(F18,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

s2224f <- s2224f %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(SkolID_gammal = NA, .before = "SkolSDO") %>%
  add_column(DIDkommun = 'Stockholm')

# names(df.sthlm)
# names(s2224f)
#write_parquet(s2224f,paste0(datafolder,"DID_klart/2024-09-12_DataPreRecode_Sthlm2024.parquet"))
#df <- s2224f
df.sthlm <- rbind(df.sthlm,s2224f)



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

### vtuna 2024 ----------------------
df.vtuna24 <- read.spss(paste0(datafolder,"Vallentuna/Sthlmsenk/Stockholmsenkäten 2024 Vallentuna (1).sav"),
                                                 to.data.frame = TRUE)

#names(df.vtuna24)
# looks like vtuna24 suffers from the same issues as Järfälla 2024 maybe?

recode_map <- read_csv("Sthlmsenk/origo2024_recode_map.csv")
#glimpse(recode_map)
#recode_vec <- setNames(recode_map$itemnr_old, recode_map$itemnr_new)

all_vars_old <- c(demogr.vars,allAnalyzedItems$itemnr)
all_vars_old[7] <- "F2"

rmap <- recode_map %>%
  mutate(itemnr_old = factor(itemnr_old, levels=unique(all_vars_old))) %>%
  arrange(itemnr_old)

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
  mutate(F18 = pmax(F19a,F19b)) %>%
  # recode back to character categories for later recoding to work
  mutate(F18 = recode(F18,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

df.vtuna24 <- df.vtuna24 %>%
  select(all_of(rmap$itemnr_new)) %>%
  rename(Kön = F2) %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Vallentuna')

names(df.vtuna24) <- names(df.vtuna)

df.vtuna <- rbind(df.vtuna,df.vtuna24)

#write_parquet(df.vtuna24,paste0(datafolder,"DID_klart/2024-10-08_DataPreRecode_Vallentuna2024.parquet"))

#df <- df.vtuna24

## Vaxholm ----------------------------------------------------------

df.vaxholm <- read.spss(paste0(datafolder,"Vaxholm/Sthlmsenk/Stockholmsenkäten 2008-2022 Vaxholm (1).sav"),
                        to.data.frame = TRUE)

df.vaxholm <- df.vaxholm %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Vaxholm')

## Danderyd ----------------------------------------------------------------

df.danderyd <- read.spss(paste0(datafolder,"Danderyd/Stockholmsenkäten 2014-2022 Danderyd.sav"),
                         to.data.frame = TRUE)

df.danderyd <- df.danderyd %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Danderyd')


## Täby --------------------------------------------------------------------

df.täby <- read.spss(paste0(datafolder,"Täby/Stockholmsenkäten 2016 & 2020-2022 Täby (2).sav"),
                     to.data.frame = TRUE)

df.täby <- df.täby %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Täby')

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


## Sigtuna -----------------------------------------------------------------

df.sigtuna <- read.spss(paste0(datafolder,"Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (9).sav"),
                        to.data.frame = TRUE)
#df.sigtuna2 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (4).sav"))

df.sigtuna <- df.sigtuna %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = 'Sigtuna')



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
             DIDkommun = 'Järfälla')

df.jfl2 <- df.jfl2 %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Järfälla')

# 2024 (maybe make this section a separate R-file and use source()?)
df.jfl3 <- read.spss(paste0(datafolder,"Järfälla/Sthlmsenkät 2024/Stockholmsenkäten 2024 Järfälla.sav"),
                     to.data.frame = TRUE)

recode_map <- read_csv("Sthlmsenk/origo2024_recode_map.csv")
#glimpse(recode_map)
#recode_vec <- setNames(recode_map$itemnr_old, recode_map$itemnr_new)

all_vars_old <- c(demogr.vars,allAnalyzedItems$itemnr)
all_vars_old[7] <- "F2"

rmap <- recode_map %>%
  mutate(itemnr_old = factor(itemnr_old, levels=unique(all_vars_old))) %>%
  arrange(itemnr_old)

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
  mutate(F18 = pmax(F19a,F19b)) %>%
  # recode back to character categories for later recoding to work
  mutate(F18 = recode(F18,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

# df.jfl3 %>%
#   distinct(F19b)

df.jfl3r <- df.jfl3 %>%
  select(all_of(rmap$itemnr_new)) %>%
  rename(Kön = F2) %>%
  add_column(SkolID_gammal = NA,
             SkolSDO = NA,
             DIDkommun = 'Järfälla')

#df.jfl3r %>%
#  rename(any_of(recode_vec))

names(df.jfl3r) <- names(df.jfl1)

df.jfl <- rbind(df.jfl1,df.jfl2,df.jfl3r)

### the SPSS file that Origo supplies for 2024 has changed case in variable names (f has become F), which causes problems in selection of variables
### let's try to make new versions of the vectors with variable names and see if we can get away with that
### solved(?) with https://stackoverflow.com/questions/11605564/r-regex-gsub-separate-letters-and-numbers
# demogr.vars2 <- gsub("f([1-9])","F\\1",demogr.vars)
# itemnr2 <- gsub("f([1-9])","F\\1",allAnalyzedItems$itemnr)
#
# df.jfl3test <- df.jfl3 %>%
#   rename(Kön = F2) %>%
#   select(any_of(c(demogr.vars2,"Arskurs",itemnr2,"SkolID_gammal","SkolSDO"))) %>%
#   add_column(SkolID_gammal = NA,
#              SkolSDO = NA,
#              DIDkommun = 'Järfälla') %>%
#   rename(ARSKURS = Arskurs) %>%
#   relocate(ARSKURS, .before = "Kön")
#
# names(df.jfl2)
# names(df.jfl3test)
# df.jfl3 %>%
#   select(starts_with("f66", ignore.case=T)) %>%
#   names()

# read new codebook since Origo Group changed virtually all variable names for 2024 data:
# s24 <- read_excel("Sthlmsenk/Kodbok Stockholmsenkäten 2024.xlsx")
#
# # create dataframes with old and new variable short names and their respective questionnaire items
# labels_new <- s24[,1:2]
# names(labels_new) <- c("itemnr_new","item_new")
# labels_old <- allAnalyzedItems[,1:2]
# names(labels_old) <- c("itemnr_old","item_old")

# labels_new %>%
#   filter(is.na(itemnr_new))
#
# # we need to remove NA from new file
# labels_new <- na.omit(labels_new)
#
# # and remove all "dik" variables
# labels_new <- labels_new %>%
#   filter(!str_detect(pattern = "dik",itemnr_new))
#
# # and remove all digits from item descriptions in the new
# labels_new2 <- labels_new
# labels_new2$item_new <- gsub('[[:digit:]]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]][a-z]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]][a-z][a-z]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]][[:digit:]]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]][[:digit:]][a-z]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]][[:digit:]][a-z][a-z]. ', '',labels_new2$item_new)
# labels_new2$item_new <- gsub('[[:digit:]]', '',labels_new2$item_new)

# use this to join automatically based on items, most are correct
# library(fuzzyjoin)
# labels <- stringdist_left_join(labels_old, labels_new2,
#                                by = c("item_old" = "item_new"),
#                                max_dist = 4)
#
# write_csv(labels,"Sthlmsenk/new_labels_mapped.csv")
# write_csv(labels_new2, "Sthlmsenk/new_labels_cleaned.csv")
# # names(df.jfl3)[1:100]
# # names(df.jfl1)[1:100]
# labels_map <- read_csv("Sthlmsenk/new_labels_mapped_manualfix.csv")
# # det är för många rader i filen jämfört med allAnalyzedItems
# labels_map %>% filter(duplicated(itemnr_old))
# # manuell fix
#
# labels_map <- read_csv("Sthlmsenk/new_labels_mapped_manualfix.csv")
# en för lite?
# allAnalyzedItems %>% filter(duplicated(itemnr))
# # oops, en dubblett i masterfilen!
# allAnalyzedItems %>%
#   rownames_to_column() %>%
#   filter(str_detect(itemnr,"F61"))
# samma är med i två Index, vi låter det vara

### vi måste även kolla demografiska variabler
# df.jfl3 %>% select(all_of(demogr.vars))
# de har också bytt namn...

# se till att vi kan koda om variabelnamnen i nya datafilen
# dvars_old <- data.frame(itemnr_old = demogr.vars)
# dvars_map <- stringdist_left_join(dvars_old, labels_new2,
#                                by = c("itemnr_old" = "itemnr_new"),
#                                max_dist = 1)
# write_csv(dvars_map,"Sthlmsenk/dvars_map.csv")

# dvars_mapped <- read_csv("Sthlmsenk/dvars_map_manualfix.csv")
# names(dvars_mapped) <- names(labels_map)
# #dvars_mapped %>% filter(duplicated(itemnr_old))
# origo2024_recode_map <- rbind(labels_map,dvars_mapped)
# write_csv(origo2024_recode_map, "Sthlmsenk/origo2024_recode_map.csv")

#df <- df.jfl3r
#df_2024 <- df
#df <- df.jfl1
# df_old <- df
#
# df_old %>%
#   select(!where(is.numeric)) %>%
#   ncol()
#
# df_2024 %>%
#   select(!where(is.numeric)) %>%
#   ncol()
#
# # 2 variables that did not recode as expected(?)
# charvars_2024 <- df_2024 %>%
#   select(!where(is.numeric)) %>%
#   names()
# charvars_old <- df_old %>%
#   select(!where(is.numeric)) %>%
#   names()
#
# setdiff(charvars_2024,charvars_old)
# F61 and F63 - one due to new response options, one because of a mistake in the recode_map, now fixed in csv file


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
  add_column(FNY22020 = NA, .after = "F40")

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
  add_column(FNY22020 = NA, .after = "F40")


df.lid3 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2020 Lidingö.sav"),
                     to.data.frame = TRUE) %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  )

df.lid4 <- read.spss(paste0(datafolder,"Lidingö/Stockholmsenkäten 2022 Lidingö.sav"),
                     to.data.frame = TRUE) %>%
  rename(Kön = F2) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  )

df.lidingö <- rbind(df.lid1,
                    df.lid2,
                    df.lid3,
                    df.lid4)


## Botkyrka ----------------------------------------------------------------

df.botkyrka <- read.spss(paste0(datafolder,"Botkyrka/Stockholmsenkäten 2004-2022 Botkyrka (4).sav"),
                                to.data.frame = TRUE)

df.botkyrka <- df.botkyrka %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Botkyrka")

# df.botkyrka %>%
#   filter(ARSKURS == "Åk 9") %>%
#   group_by(ar) %>%
#   summarise(na = sum(is.na(Skolenhetskod)))


## Haninge -----------------------------------------------------------------

haninge <- read.spss("/Volumes/rise/Gemensam/Framtidens socialtjänst/Data från Haninge/Stockholmsenkäten 2002-2022 Haninge.sav", to.data.frame = TRUE)
df.haninge <- haninge %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Haninge")



## Sundbyberg --------------------------------------------------------------

sundbyberg <- read.spss(paste0(datafolder,"Sundbyberg/Sundbyberg Rådata/Stockholmsenkäten 2002-2022 Sundbyberg.sav"), to.data.frame = TRUE)

df.sundbyberg <- sundbyberg %>%
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>%
  add_column(DIDkommun = "Sundbyberg")


#df <- rbind(df.haninge,df.sundbyberg)

### compare within municipality
# setdiff(names(df.jfl1),names(df.jfl2))
# setdiff(names(df.jfl2),names(df.jfl1))
### compare to template
#setdiff(names(df),names(df.lid3))
#setdiff(names(df.lid3),names(df))


# df <- df %>%
#   rename(`Hur länge har du bott i Sverige?` = F5,
#          `Vilken högsta utbildning har din mamma?` = f6a,
#          `Vilken högsta utbildning har din pappa?` = f6b,
#          `Vad bor du i för typ av bostad?` = F7)
# df.old <- read_parquet("../DIDapp/data/SthlmsEnkRev_2022-12-20.parquet")
# df.old <- df.old %>%
#   rename(Community = Närsamhälle,
#          Parenting = Föräldraskap,
#          PsykSomBesv = 'Psykiska/ psykosomatiska besvär',
#          SkolaNegativ = 'Vantrivsel i skolan',
#          Wellbeing = Välbefinnande,
#          SkolaPositiv = 'Positiv skolanknytning'
#   )
# df.old <- df.old %>%
#   rename(DIDkommun = Kommun)

# write_parquet(df.new, sink = glue("../data/{Sys.Date()}_ScoredRev.parquet"))

# Combine all data --------------------------------------------------------

## find out which, if any, variables differ between df's
# df.sthlm %>%
#  select_at(vars(names(df.vaxholm)))

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
            df.sundbyberg)

#write_parquet(df,paste0(datafolder,"DID_klart/2024-09-12_DataPreRecode.parquet"))
#write_parquet(df.jfl3r,paste0(datafolder,"DID_klart/2024-08-22_DataPreRecode_Järfälla2024.parquet"))

# create data frame with 0 rows and named variables as a template
#names <- data.frame(matrix(ncol = length(names(df)), nrow = 0))
# provide column names
#colnames(names) <- names(df)
#write_csv(names, "Sthlmsenk/variabler.csv")

## On to next script! 02 for recodings.
