

## Setting up

library(httr)
library(arrow)
library(tidyverse)
library(rjson)
library(jsonlite)
library(readxl)
library(glue)


# Add the municipalities we are interested in (municipalities in Stockholm and Uppsala regions).

municipalities <- read_parquet("Skolverket/2023-03-28_KOLADA_Municipality_list.parquet") %>%
  filter(str_detect(id, "^01|^03")) %>%
  select(!type)

# Create a loop where we fetch data for all municipalities we are interested in. We use "municipality" as loop factor. We need two different functions to fetch data (one for gr and fsk and one for gy schools). Let's define them before the loop.

# For gr and fsk:
oneSchool <- function(listN) {
  schoolUnit <- schoolData[[listN]]$body$schoolUnit
  vars <- which(sapply(schoolData[[listN]]$body, function(x) length(x) > 1)) # added line of code
  df_total <- data.frame()

  for (i in names(vars)) {
    tmp <- schoolData[[listN]]$body %>%
      pluck(i) %>%
      data.frame() %>%  # Convert to data frame
      add_column(variable = i,
                 Skolenhetskod = schoolUnit)
    df_total <- rbind(df_total, tmp)
  }
  return(df_total)
}

# For gy:
oneSchool_gy <-  function(listNr) {
  schoolUnit <- schoolData[[listNr]]$body$programMetrics$schoolUnit[1]

  vars <- character(0)

  for (i in names(schoolData[[listNr]]$body)) {
    if (length(schoolData[[listNr]]$body[[i]]) > 1 && !grepl("^programMetrics", i)) {
      vars <- c(vars, i)
    }
  }

  df_total <- data.frame()

  for (i in vars) {
    tmp <- schoolData[[listNr]]$body %>%
      pluck(i) %>%
      data.frame() %>%  # Convert to data frame
      add_column(variable = i,
                 Skolenhetskod = schoolUnit)
    df_total <- rbind(df_total, tmp)
  }
  return(df_total)
}

# <!-- Create a short list for testing the loop. -->
#
# <!-- -->
# <!-- test_munici <- municipalities$id[1:2] -->
# <!--  -->

## The loop

# First create the data frame where all data will be stored

df.all <- data.frame()

# Then loop over all municipalities and save all data into the df.all data frame as new rows.

for (j in municipalities$id) {
  # Get schooldata for all schools in each municipality
  data <- (GET(paste0("https://api.skolverket.se/skolenhetsregistret/v1/kommun/", j))) %>%
  content("text") %>%
  fromJSON()

  # save alla school units
  schools <- data$Skolenheter

  # Check what schools have data in the database and which do not
  schoolsAvailable <- data.frame()
  for (i in schools$Skolenhetskod) {
  tmp <- http_status(GET(paste0("https://api.skolverket.se/planned-educations/v3/school-units/", i))) %>%
    pluck("reason") %>%
    as.data.frame(nm = "status") %>%
    add_column(Skolenhetskod = i)
  schoolsAvailable <- rbind(schoolsAvailable, tmp)
}

  # Remove schools unavailable in the database
  schoolsFiltered <- schools %>%
  left_join(.,schoolsAvailable, by = "Skolenhetskod") %>%
  filter(status == "OK") %>%
  select(!status)

  # Create schoolTypes to get type of schooling.
  schoolTypes <- data.frame()

  for (i in schoolsFiltered$Skolenhetskod) {

  tmp <- GET(paste0("https://api.skolverket.se/planned-educations/v3/school-units/", i)) %>%
    content("text") %>%
    fromJSON()
  tmp2 <- data.frame(
    type = tmp$body$typeOfSchooling$code,
    Skolenhetskod = i)

  schoolTypes <- rbind(schoolTypes,tmp2)
}

  # Retrieve data for all schools in each municipality
  schoolData <- map2(
  .x = schoolTypes$Skolenhetskod,
  .y = schoolTypes$type,
  ~ GET(paste0("https://api.skolverket.se/planned-educations/v3/school-units/", .x,"/statistics/", .y)) %>%
    content("text") %>%
    fromJSON()
  )

  ##### Gr schools

  # identify all gr school units
  mapSchoolUnits <- schoolTypes %>%
  rownames_to_column() %>%
  filter(type == "gr") %>%
  pull(rowname) %>%
  as.numeric()

  # Remove the values from mapschoolunits where status is "not_found" in shoolData
  mapSchoolUnits <- mapSchoolUnits %>%
    keep(function(i) {
      status <- schoolData[[i]]$status
      status == "OK"
    }
    )

  # Save data for all gr schools in a nested data frame
  nestedOutputGR <- map(mapSchoolUnits, ~ oneSchool(.x))

  # Unnest the data
  unnestedOutputGR <- map_dfr(c(1:length(nestedOutputGR)), ~ do.call(bind_rows, nestedOutputGR[[.x]]))

  #Recoding

  # create copy of dataframe prior to recoding
  df.gr <- unnestedOutputGR

  # recode . to NA
  df.gr$value <- car::recode(df.gr$value,"'.'=NA")
  # change decimal comma to decimal point
  df.gr$value <- gsub(pattern = ",",
                      replacement = ".",
                      df.gr$value)

  # remaining cleanup
  df.gr$value <- car::recode(df.gr$value,"'..'=NA") # too few pupils
  df.gr$value <- car::recode(df.gr$value,"'*'=NA") # no req for teacher license
  df.gr$value <- gsub("cirka ", "", df.gr$value)
  df.gr$value <- gsub("~", "", df.gr$value)

  df.gr$value <- as.numeric(df.gr$value)

  # date variable
  df.gr$timePeriod <- str_extract(df.gr$timePeriod, "\\d{4}")
  df.gr$timePeriod <- as.integer(df.gr$timePeriod)

  df.gr %>%
    count(timePeriod)

  # we like to use school names instead of codes when creating figures
  df.gr <- left_join(df.gr,schools, by = "Skolenhetskod")

  # Add school type
  df.gr <- df.gr %>%
    left_join(filter(schoolTypes, type == "gr"), by = "Skolenhetskod")

  #### Fsk schools

  # Identify all fsk school units
  mapSchoolUnits_fsk <- schoolTypes %>%
  rownames_to_column() %>%
  filter(type == "fsk") %>%
  pull(rowname) %>%
  as.numeric()

  # Remove the values from mapschoolunits_fsk where status is "not_found" in shoolData
  mapSchoolUnits_fsk <- mapSchoolUnits_fsk %>%
    keep(function(i) {
      status <- schoolData[[i]]$status
      status == "OK"
    }
    )

  # Save data for all fsk schools in a nested data frame
  nestedOutputFSK <- map(mapSchoolUnits_fsk, ~ oneSchool(.x))

  # Unnest the data
  unnestedOutputFSK <- map_dfr(c(1:length(nestedOutputFSK)), ~ do.call(bind_rows, nestedOutputFSK[[.x]]))

  #Recoding

   # create copy of dataframe prior to recoding
  df.fsk <- unnestedOutputFSK

  # recode . to NA
  df.fsk$value <- car::recode(df.fsk$value,"'.'=NA")
  # change decimal comma to decimal point
  df.fsk$value <- gsub(pattern = ",",
                      replacement = ".",
                      df.fsk$value)

  # remaining cleanup
  df.fsk$value <- car::recode(df.fsk$value,"'..'=NA") # too few pupils
  df.fsk$value <- car::recode(df.fsk$value,"'*'=NA") # no req for teacher license
  df.fsk$value <- gsub("cirka ", "", df.fsk$value)
  df.fsk$value <- gsub("~", "", df.fsk$value)

  df.fsk$value <- as.numeric(df.fsk$value)

  # date variable
  df.fsk$timePeriod <- str_extract(df.fsk$timePeriod, "\\d{4}")
  df.fsk$timePeriod <- as.integer(df.fsk$timePeriod)

  df.fsk %>%
    count(timePeriod)

  # we like to use school names instead of codes when creating figures
  df.fsk <- left_join(df.fsk,schools, by = "Skolenhetskod")

  # Add school type
  df.fsk <- df.fsk %>%
    left_join(filter(schoolTypes, type == "fsk"), by = "Skolenhetskod")

  #### Gy schools

  # Identify all gy school units
  mapSchoolUnits_gy <- schoolTypes %>%
  rownames_to_column() %>%
  filter(type == "gy") %>%
  pull(rowname) %>%
  as.numeric()

  # Remove the values from mapschoolunits_gy where status is "not_found" in shoolData
  mapSchoolUnits_gy <- mapSchoolUnits_gy %>%
    keep(function(i) {
      status <- schoolData[[i]]$status
      status == "OK"
    }
    )

  # Save data for all gy schools in a nested data frame
  if (!is.null(mapSchoolUnits_gy) && length(mapSchoolUnits_gy) > 0) { # This if statement checks if mapSchoolunits_gy is empty
    nestedOutputGY <- map(mapSchoolUnits_gy, ~ oneSchool_gy(.x))
  } else {
    print("mapSchoolUnits_gy is empty.")
  }

  # Unnest the data
  unnestedOutputGY <- map_dfr(c(1:length(nestedOutputGY)), ~ do.call(bind_rows, nestedOutputGY[[.x]]))

  #Recoding

   # create copy of dataframe prior to recoding
  df.gy <- unnestedOutputGY

  # recode . to NA
  df.gy$value <- car::recode(df.gy$value,"'.'=NA")
  # change decimal comma to decimal point
  df.gy$value <- gsub(pattern = ",",
                      replacement = ".",
                      df.gy$value)

  # remaining cleanup
  df.gy$value <- car::recode(df.gy$value,"'..'=NA") # too few pupils
  df.gy$value <- car::recode(df.gy$value,"'*'=NA") # no req for teacher license
  df.gy$value <- gsub("cirka ", "", df.gy$value)
  df.gy$value <- gsub("~", "", df.gy$value)

  df.gy$value <- as.numeric(df.gy$value)

  # date variable
  df.gy$timePeriod <- str_extract(df.gy$timePeriod, "\\d{4}")
  df.gy$timePeriod <- as.integer(df.gy$timePeriod)

  df.gy %>%
    count(timePeriod)

  # we like to use school names instead of codes when creating figures
  df.gy <- left_join(df.gy,schools, by = "Skolenhetskod")

  # Add school type
  df.gy <- df.gy %>%
    left_join(filter(schoolTypes, type == "gy"), by = "Skolenhetskod")

  ### Combine data frames of all school types

  df.fsk.gr.gy <- rbind(df.fsk, df.gr, df.gy)

  # Add municipality name
  df.fsk.gr.gy <- left_join(df.fsk.gr.gy, municipalities, by = c("Kommunkod" = "id"))

  # Rename "title" to "Kommunnamn"
  df.fsk.gr.gy <- df.fsk.gr.gy %>%
    rename(Kommunnamn = title)

  # Add all data for municipality j to the df.all
  df.all <- rbind(df.all, df.fsk.gr.gy)

}

## Save data frame to a parquet file

# Write the dataframe to a compressed file. Parquet is a really fast and efficient open format that is included in `library(arrow)`.

write_parquet(df.all,glue("{Sys.Date()}_SkolverketData-all_municipalities_in_Stockholm_and_Uppsala.parquet"))


















