library(tidyverse)
library(janitor)

df <- read_delim("Skolverket/Grundskolan - Antal elever per årskurs 2009 Skolenhet.csv",
                 skip = 5, na = c(".",".."), delim = ";") %>%
  clean_names() %>%
  select(!x23) %>%
  add_column(year = 2010)

# get all filenames in the folder
fileList <- list.files(path = "Skolverket/", full.names=TRUE)
years <- c(2010:2023)
# create an empty list to store data in
data <- list()
# loop over filenames to import them to the list object
for (i in c(1:14)) {
  data[[i]] <- read_delim(fileList[i],
                          skip = 5, na = c(".",".."), delim = ";") %>%
    clean_names() %>%
    select(!x23) %>%
    add_column(year = years[i]) %>%
    mutate(elever_arskurs_1_9 = as.numeric(elever_arskurs_1_9),
           skol_enhetskod = as.numeric(skol_enhetskod),
           andel_percent_elever_med_utlandsk_bakgrund_ak_1_9 = as.numeric(andel_percent_elever_med_utlandsk_bakgrund_ak_1_9))
}

### Combine all nested lists into one dataframe
df <- map_dfr(c(1:14), ~ do.call(bind_rows, data[[.x]]))

library(arrow)
write_parquet(df,"Skolverket/skolverketDemografi.parquet")
