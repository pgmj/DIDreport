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
