library(tidyverse)
library(janitor)
library(glue)
# manual download of data from https://www.skolverket.se/skolutveckling/statistik/sok-statistik-om-forskola-skola-och-vuxenutbildning
# file naming is based on the fall term, meaning that 2022 indicates the school year 2022/2023.
test <- read_delim("Skolverket/Grundskolan - Antal elever per årskurs 2022 Skolenhet.csv",
                 skip = 5, na = c(".",".."), delim = ";") %>%
  clean_names() %>%
  select(!x23) %>%
  add_column(year = 2010) %>%
  mutate(across(starts_with("andel"), ~ gsub(",",".",.x)))


glimpse(test)

# get all filenames in the folder
fileList <- list.files(path = "Skolverket/", full.names=TRUE, pattern = "Grundskolan")
# since Stockholmsenkäten collects data during the spring term, we'll adapt the year variable to fit their structure and use the latter half of the school year
years <- c(2010:2023)
# create an empty list to store data in
data <- list()
# loop over filenames to import them to the list object
for (i in c(1:length(fileList))) {
  data[[i]] <- read_delim(fileList[i],
                          skip = 5, delim = ";") %>%
    clean_names() %>%
    select(!x23) %>%
    add_column(year = years[i]) %>%
    mutate(across(elever_forskoleklass:elever_arskurs_9, ~ car::recode(.x,"'.'=NA;'..'=NA"))) %>%
    mutate(across(starts_with("andel"), ~ gsub(",",".",.x))) %>%
    mutate(across(starts_with("andel"), ~ as.numeric(.x))) %>%
    mutate(elever_arskurs_1_9 = as.numeric(elever_arskurs_1_9),
           skol_enhetskod = as.numeric(skol_enhetskod))
}


#map(c(1:14), ~ head(data[[.x]]$andel_percent_flickor_arskurs_1_9),5)

### Combine all nested dataframes into one dataframe
df <- map_dfr(c(1:14), ~ do.call(bind_rows, data[[.x]]))

library(arrow)
write_parquet(df,glue("{Sys.Date()}_skolverketDemografi.parquet"))
