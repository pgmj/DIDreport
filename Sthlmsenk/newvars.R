### This is a lookup table for the variable names used in our analyses
recode_map <- read_csv("Sthlmsenk/origo2024_recode_map2.csv")

### We need to change variable names in the new data to match the old data, since all other scripts rely on the old variable names


### We also have new variables about tobacco use that need handling.

# ## F14a och F14b angår cigaretter m tobak och e-cigaretter, d.v.s:
# # F14a = F14, och F14b = FNY12020
# ## F18a och F18b angår vitt snus och snus med tobak (OBS omvänt från F14)
# s2224 %>%
#   count(ar,F14b) %>% # F14b och F18a har varit med sedan 2022
#   as.data.frame()

df.vtuna24b <-
  df.vtuna24 %>%
  # rename(F14 = F14a,
  #        FNY12020 = F14b) %>%
  mutate(across(c(F19a,F19b), ~ recode(.x,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=3;
                 'Ja, ibland men inte varje dag'=4;
                 'Ja, dagligen'=5;
                 '<NA>'=NA",
                                       as.factor = F))) %>%
  #mutate(across(c(F19a,F19b), ~ factor(.x, ordered = TRUE))) %>%
  mutate(F19 = pmax(F19a,F19b)) %>%
  # recode back to character categories for later recoding to work
  mutate(F19 = recode(F19,"0='Nej, jag har aldrig snusat';
                 1='Nej, bara provat hur det smakar';
                 2='Nej, jag har snusat men slutat';
                 3='Nej, jag har slutat';
                 4='Ja, ibland men inte varje dag';
                 5='Ja, dagligen'"))

### we want to recode the new names to become the old names
recode_map_sorted <- recode_map %>%
  arrange(itemnr_new)

dupes_new <- c("F35","F45","F59","F81","F91","F93","F95")
#dupes_old <- c("F34","F44","F58","F79","F89")

rec_map2 <- recode_map_sorted %>%
  filter(!itemnr_new %in% dupes_new)

rec_map3 <- rec_map2 %>%
  filter(!itemnr_old %in% dupes_new)

recode_vec <- setNames(rec_map3$itemnr_new, rec_map3$itemnr_old)

df.vtuna24b %>%
  dplyr::rename(any_of(recode_vec)) %>%
  glimpse() # for better printing


recode_map %>%
  filter(itemnr_old %in% dupes)



recode_map %>%
  filter(itemnr_new %in% dupes_new)

recode_map %>%
  count(itemnr_old) %>%
  filter(n > 1)
