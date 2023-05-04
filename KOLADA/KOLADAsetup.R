library(rKolada)
library(arrow)
library(tidyverse)
library(glue)


# Retrieve KPI and municipality lists if needed ---------------------------

# kpis <- get_kpi(cache = FALSE)
# write_parquet(kpis, glue("{Sys.Date()}_KOLADA_KPI_list.parquet"))

# munic <- get_municipality(cache = FALSE) %>%
#   filter(type == "K")
# write_parquet(munic, glue("{Sys.Date()}_KOLADA_Municipality_list.parquet"))

## Read downloaded information from disk to avoid API abuse -------------------------------------

kpis <- read_parquet("")
munic <- read_parquet("KOLADA/2023-03-28_KOLADA_Municipality_list.parquet")

sthlms.län <- munic %>%
  filter(str_detect(id,"^01")) # filter to only include municipalities that begin with "01" for Stockholms län
# 03 is Uppsala

KPI <- select(kpis, id, title)


# Download data via API ---------------------------------------------------

# This call takes a long time, and should only be used when updating the KPI
# selection or when new data is available. Otherwise, skip ahead and read
# downloaded datafile instead

# df.values <- get_values(
#   kpi = c(
#     "U09808", "U09800", "N09805", "N09890", "N02904", "N07907",
#     "N07914", "U07452", "U07417", "N17625", "N15428",
#     "N15816", "N17461", "N11821", "N11050", "N11052", "N11051",
#     "N15813", "N17538", "N15030", "N17813", "N00943", "N00945"
#   ),
#   municipality = munic$id,
#   period = 2006:2022, simplify = TRUE
# )
#write_parquet(df.values, glue("{Sys.Date()}_KOLADA_data_raw.parquet"))

## Read data from disk -------------------------------------------------------

df.values <- read_parquet("2023-03-28_KOLADA_data_raw.parquet")


# Data wrangling ----------------------------------------------------------


# Remove unnecessary columns

df.values$count <- NULL
df.values$status <- NULL
df.values$municipality_type <- NULL

# Rename column names to match

colnames(KPI) <- c("kpi", "KPI")

# Add description to df.values based on kpi

df.values <- left_join(df.values, KPI, by = "kpi")

# Reorder columns to match Magnus'

df.values <- df.values %>%
  select(municipality, municipality_id, KPI, kpi, year, value, gender)

# Drop municipality id

df.values$municipality_id <- NULL

# Rename columns to match Magnus'

colnames(df.values) <- c("Kommun", "KPI", "kpi", "År", "Andel", "Kön")

# Recode Factor names to match Magnus'

df.values$Kön <- recode_factor(df.values$Kön, T = "Alla",
                               M = "Pojke", K = "Flicka")

# check that things look good
# df.values %>%
#   distinct(kpi,KPI) %>%
#   kbl()

write_parquet(df.values, glue("{Sys.Date()}_KOLADA_data_ready.parquet"))

