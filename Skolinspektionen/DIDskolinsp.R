library(ggplot2)
library(tidyverse)
library(arrow)
library(colorspace) # for modifying color palettes
library(formattable)
library(geomtextpath)
library(readxl)
library(visNetwork)
library(networkD3)
library(vctrs)
library(ggiraph)
library(gt)
library(gtExtras)
library(RISEkbmRasch)
library(janitor)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

LänetsKommuner <- read_parquet("KOLADA/2023-03-28_KOLADA_Municipality_list.parquet") %>%
  filter(str_detect(id,"^01")) %>%
  pull(title)

# manual download from https://skolinspektionen.se/beslut-rapporter-statistik/statistik/statistik-fran-skolenkaten/resultat-skolenkaten-2022/
df.si5 <- read_excel("Skolinspektionen/excelrapport-elever-grundskola-ak-5-2022.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(x3),
         str_detect(x2,"kommun")) %>%
  # subset municipality name, total N, and n of responses for trygghet and nöjd
  select(x2,x6,x7,x8,x303,x305,x307,x309,x311,x363,x365,x367,x369,x371) %>%
  rename(Kommun = x2) %>%
  mutate(across(starts_with("x"), ~ as.numeric(.x))) %>% # make numeric variables numeric
  rename(TotalN = x6,
         AntalN = x7,
         Svarsfrekvens = x8,
         trygghet_helt = x303,
         trygghet_stor = x305,
         trygghet_viss = x307,
         trygghet_inte = x309,
         trygghet_vetinte = x311,
         nöjdhet_helt = x363,
         nöjdhet_stor = x365,
         nöjdhet_viss = x367,
         nöjdhet_inte = x369,
         nöjdhet_vetinte = x371) %>%
  mutate(Kommun = gsub(" kommun", "", Kommun),
         Svarsfrekvens = round(Svarsfrekvens, 3)) %>% # remove the word "kommun"
  add_column(Årskurs = "Åk 5")

df.si8 <- read_excel("Skolinspektionen/excelrapport-elever-grundskola-ak-8-2022.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(x3),
         str_detect(x2,"kommun")) %>%
  # subset municipality name, total N, and n of responses for trygghethet and nöjdhet
  select(x2,x6,x7,x8,x303,x305,x307,x309,x311,x363,x365,x367,x369,x371) %>%
  rename(Kommun = x2) %>%
  mutate(across(starts_with("x"), ~ as.numeric(.x))) %>% # make numeric variables numeric
  rename(TotalN = x6,
         AntalN = x7,
         Svarsfrekvens = x8,
         trygghet_helt = x303,
         trygghet_stor = x305,
         trygghet_viss = x307,
         trygghet_inte = x309,
         trygghet_vetinte = x311,
         nöjdhet_helt = x363,
         nöjdhet_stor = x365,
         nöjdhet_viss = x367,
         nöjdhet_inte = x369,
         nöjdhet_vetinte = x371) %>%
  mutate(Kommun = gsub(" kommun", "", Kommun),
         Svarsfrekvens = round(Svarsfrekvens, 3)) %>% # remove the word "kommun"
  add_column(Årskurs = "Åk 8")

df.si.g2 <- read_excel("Skolinspektionen/excelrapport-elever-gymnasieskola-ar-2-2022.xlsx", sheet = 1) %>%
  janitor::clean_names() %>%
  filter(is.na(x3),
         str_detect(x2,"kommun")) %>%
  # subset municipality name, total N, and n of responses for trygghet and nöjdhet
  select(x2,x6,x7,x8,x303,x305,x307,x309,x311,x363,x365,x367,x369,x371) %>%
  rename(Kommun = x2) %>%
  mutate(across(starts_with("x"), ~ as.numeric(.x))) %>% # make numeric variables numeric
  rename(TotalN = x6,
         AntalN = x7,
         Svarsfrekvens = x8,
         trygghet_helt = x303,
         trygghet_stor = x305,
         trygghet_viss = x307,
         trygghet_inte = x309,
         trygghet_vetinte = x311,
         nöjdhet_helt = x363,
         nöjdhet_stor = x365,
         nöjdhet_viss = x367,
         nöjdhet_inte = x369,
         nöjdhet_vetinte = x371) %>%
  mutate(Kommun = gsub(" kommun", "", Kommun),
         Svarsfrekvens = round(Svarsfrekvens, 3)) %>% # remove the word "kommun"
  add_column(Årskurs = "Gy 2")

df.si.2022 <- rbind(df.si5,df.si8,df.si.g2)

library(fuzzyjoin)
#joina med tabell med fyrsiffrigt ID för kommunerna och filtrera på den?
# which muni's name end with an s and thus may need recoding?
fuzz <- df.si.2022 %>%
  filter(str_ends(Kommun,"s")) %>%
  stringdist_left_join(KommunID, by = "Kommun", method = 'lcs') %>%
  rename(Kommun = Kommun.y) %>%
  select(!c("Kommun.x","KommunID")) %>%
  relocate(Kommun, .before = "TotalN") %>%
  mutate(Kommun = car::recode(Kommun,"NA='Dals-Ed'"))

rest <- df.si.2022 %>%
  filter(!str_ends(Kommun,"s"))

df.si.2022 <- rbind(fuzz,rest)

# write wrangled data to file
write_parquet(df.si.2022, "Skolinspektionen/Trygghet och studiero 2022/SItryggNöjd.parquet")
# df.si.2022 %>%
#  filter(Årskurs == "Åk 5") %>%
#  distinct(Kommun)
df.si.2022 <- read_parquet("Skolinspektionen/Trygghet och studiero 2022/SItryggNöjd.parquet")

# prepare visualizations --------------------------------------------------

df.si.long <- df.si.2022 %>%
  filter(Kommun %in% LänetsKommuner) %>%
  select(!TotalN) %>%
  pivot_longer(starts_with(c("trygghet","nöjdhet")),
               values_to = "Antal") %>%
  separate(name, c("item","svarskategori"), sep = "_") %>%
  group_by(Kommun,Årskurs,item) %>%
  mutate(Andel = round(100 * Antal / sum(Antal, na.rm = T),1),
         Svarsfrekvens = 100 * Svarsfrekvens) %>%
  ungroup()

DIDskolinsp <- function(item, årskurs, svarskategorier = c("helt", "stor")) {
  df.si.long %>%
    filter(
      item == {{item}},
      Årskurs == {{årskurs}}
    ) %>%
    filter(svarskategori %in% {{svarskategorier}}) %>%
    group_by(Kommun, Årskurs, item, Svarsfrekvens) %>%
    summarise(Andel = sum(Andel, na.rm = T)) %>%
    ungroup() %>%
    mutate(Kommun = fct_reorder(Kommun, Andel)) %>%

    ggplot(aes(x = Kommun, y = Andel)) +
    geom_col(aes(fill = Kommun)) +
    geom_col(data = . %>%
               filter(Kommun == fokusKommun),
             color = "black",
             fill = "darkgrey") +
    geom_col(data = . %>%
               filter(Kommun %in% jmfKommun),
             color = "darkgrey",
             fill = "lightgrey") +
    geom_hline(aes(yintercept = mean(Andel, na.rm = T)),
      linetype = 3,
      color = "#D55E00",
      linewidth = 0.9,
      alpha = 0.7
    ) +
    geom_text(aes(label = paste0(round(mean(Andel, na.rm = T),1),"%"),
                  y = mean(Andel, na.rm = T)+3),
              x = 1,
              color = "sienna2",
              alpha = 0.6) +
    geom_text(aes(label = paste0(round(Andel,1),"%")),
              position = position_dodge(width = 0.9),
              hjust = 0, vjust = -0.35, angle = 45, size = 2.9,
              color = "black") +
    geom_text(aes(label = paste0(Svarsfrekvens,"%"),
                  y = 1,
                  angle = 0),
              position = position_dodge(width = 1),
              hjust = 0.5,
              vjust = 0,
              size = 2.4,
              color = "white"
    ) +
    scale_fill_viridis_d(aesthetics = c("color","fill"),
                         guide = "none") +
    theme_rise() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
    labs(title = paste0("Skolinspektionen - ",årskurs," - 2022"),
         subtitle = glue("Andel respondenter som svarat positivt på frågan om '{item}'"),
         caption = "Siffror längst ner i kolumnen indikerar svarsfrekvensen.\nKälla: Skolinspektionens skolenkät") +
    coord_cartesian(clip = "off")
}

fokusKommun <- "Sigtuna"
jmfKommun <- c("Botkyrka","Järfälla","Upplands Väsby")

DIDskolinsp("trygghet","Åk 8")



# kolada ------------------------------------------------------------------

df.si.kolada <- read_parquet("KOLADA/2023-05-19_KOLADA_skolinsp_tryggNöjd.parquet")



# testing -----------------------------------------------------------------

df.si.long %>%
  filter(
    item == "Trygg",
    Årskurs == "Åk 5"
  ) %>%
  filter(svarskategori %in% c("helt", "stor")) %>%
  group_by(Kommun, Årskurs, item, Svarsfrekvens) %>%
  summarise(Andel = sum(Andel, na.rm = T)) %>%
  ungroup() %>%
  mutate(Kommun = fct_reorder(Kommun, Andel))

df.si.long %>%
  filter(item == "trygghet") %>%
  filter(svarskategori %in% c("helt", "stor")) %>%
  group_by(Kommun, Årskurs) %>%
  summarise(Andel = sum(Andel, na.rm = T)/100,
            Antal = sum(Antal, na.rm = T)) %>%
  ungroup() %>%

  group_by(Årskurs) %>%
  summarise(Andel = mean(Andel, na.rm = T),
            Antal = sum(Antal)) %>%
  mutate(sem = sqrt(Andel * (1 - Andel) / sum(Antal))) %>% # räkna ut standard error of measurement
  mutate(
    lower.95ci = Andel - sem * 1.96, # räkna fram nedre och högre gränsvärden för 95% CI
    upper.95ci = Andel + sem * 1.96) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

test <- df.si.2022 %>%
  filter(Kommun %in% LänetsKommuner) %>%
  select(!TotalN) %>%
  pivot_longer(starts_with(c("Trygg","Nöjd")),
               values_to = "Antal") %>%
  separate(name, c("item","svarskategori"), sep = "_") %>%
  group_by(Kommun,Årskurs,item) %>%
  mutate(Andel = 100 * Antal / sum(Antal, na.rm = T)) %>%
  filter(
    item == "Trygg",
    Årskurs == "Åk 5"
  ) %>%
  filter(svarskategori %in% c("helt", "stor")) %>%
  group_by(Kommun, Årskurs, item) %>%
  summarise(Andel = sum(Andel, na.rm = T)) %>%
  ungroup() %>%
  mutate(Kommun = fct_reorder(Kommun, Andel))

df.si.2022.sums %>%
  filter(Kommun %in% LänetsKommuner) %>%
  filter(Årskurs == "Åk 5")

test %>%
  mutate(sem = sqrt(Andel * (1 - Andel) / sum(Antal))) %>% # räkna ut standard error of measurement
  mutate(
    lower.95ci = Andel - sem * 1.96, # räkna fram nedre och högre gränsvärden för 95% CI
    upper.95ci = Andel + sem * 1.96) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
