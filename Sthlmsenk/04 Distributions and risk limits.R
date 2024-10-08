### See file rslimits2024.qmd for a more detailed view of the method applied and choices made

library(arrow)
library(tidyverse)
library(car)
library(glue)
library(janitor)
### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename
filter <- dplyr::filter

# df <- read_parquet(paste0(datafolder,"DID_klart/2023-05-07_ScoredRev.parquet")) %>%
#   filter(ar > 2004 & ar < 2022)

datafolder <- "~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/"
allItems <- read_csv("Sthlmsenk/allItemsIndex.csv")

df <- read_parquet(paste0(datafolder,"DID_klart/2024-09-12_ScoredRev.parquet")) %>%
  filter(ar > 2004 & ar < 2022)

#---- get cutoff values based on percentiles----

# create vector of risk index names
sthlm.index <- allItems %>%
  filter(!Index %in% c("Wellbeing","SkolaPositiv")) %>%
  distinct(Index) %>%
  pull()


# Risk factors ------------------------------------------------------------

# get n complete responses for index values per year
antalsvar <- df %>%
  select(ar,all_of(sthlm.index)) %>%
  na.omit() %>%
  count(ar)

p75 <- df %>%
  select(ar,all_of(sthlm.index)) %>%
  pivot_longer(all_of(sthlm.index), names_to = "index") %>%
  group_by(ar,index) %>%
  summarise(p75 = quantile(value, probs = .75, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(values_from = p75,
              names_from = index) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  left_join(antalsvar, by = "ar")

p90 <- df %>%
  select(ar,all_of(sthlm.index)) %>%
  pivot_longer(all_of(sthlm.index), names_to = "index") %>%
  group_by(ar,index) %>%
  summarise(p90 = quantile(value, probs = .90, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(values_from = p90,
              names_from = index) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  left_join(antalsvar, by = "ar")

p90w <- lapply(p90[c(sthlm.index)], function(x) weighted.mean(x,p90$n)) %>%
  unlist() %>%
  as.data.frame(nm = "p90")

p75w <- lapply(p75[c(sthlm.index)], function(x) weighted.mean(x,p75$n)) %>%
  unlist() %>%
  as.data.frame(nm = "p75")

rslimits <- cbind(p75w,p90w) %>%
  t() %>%
  as.data.frame()

#write_csv(rslimits, file = "2024-09-23_rslimitsNoRev.csv")

# cutoff values for protective factors -------------------------------------
# scores are reversed, making higher score = positive


# new code

p85 <- df %>%
  select(ar,all_of(andrafaktorer)) %>%
  pivot_longer(all_of(andrafaktorer), names_to = "index") %>%
  group_by(ar,index) %>%
  summarise(p85 = quantile(value, probs = .85, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(values_from = p85,
              names_from = index) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  left_join(antalsvar, by = "ar")

p15 <- df %>%
  select(ar,all_of(andrafaktorer)) %>%
  pivot_longer(all_of(andrafaktorer), names_to = "index") %>%
  group_by(ar,index) %>%
  summarise(p15 = quantile(value, probs = .15, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(values_from = p15,
              names_from = index) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  left_join(antalsvar, by = "ar")

p85w <- lapply(p85[c(andrafaktorer)], function(x) weighted.mean(x,p85$n)) %>%
  unlist() %>%
  as.data.frame(nm = "p85")

p15w <- lapply(p15[c(andrafaktorer)], function(x) weighted.mean(x,p85$n)) %>%
  unlist() %>%
  as.data.frame(nm = "p15")

rslimitsProt <-
#rs <-
  cbind(p15w,p85w) %>%
  t() %>%
  as.data.frame()


#write_csv(rslimitsProt, file = "Sthlmsenk/2024-09-23_protective.csv")

