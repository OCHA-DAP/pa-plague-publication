###############
#### SETUP ####
###############

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(ISOweek)
library(zoo)

# data sourced from AA directory and saved to publication directory
dir_aa <- Sys.getenv("AA_DATA_DIR")
dir_pub <- Sys.getenv("MDG_PLAGUE_PUB_DIR")
dir_plot_save <- here(dir_pub, "plots")
dir_data_save <- here(dir_pub, "data")

# urban classification of admin3 areas
fp_urban <- file.path(
  dir_aa,
  "public",
  "processed",
  "mdg",
  "urban_classification",
  "mdg_adm3_urban_classification.csv"
)

# plague case data
dir_plague <- file.path(
  dir_aa,
  "private",
  "raw",
  "mdg",
  "institut_pasteur"
)

# urban classifications for adm3 areas
fp_urban_class <- file.path(
  dir_aa,
  "public",
  "processed",
  "mdg",
  "urban_classification",
  "mdg_adm3_urban_classification.csv"
)

# get old cases back to 2012 and new cases
fp_historic <- file.path(
  dir_plague,
  "Madagascar_IPM_Plague_cases_Aggregated_historic_2021-10-18.csv"
)

fp_new <- file.path(
  dir_plague,
  "Madagascar_IPM_Plague_cases_Aggregated_2022-04-04_checked for OCHA_v1.xls"
)

# save paths
fp_cases_detailed <- file.path(
  dir_data_save,
  "case_data_detailed.csv"
)

fp_cases_urban <- file.path(
  dir_data_save,
  "case_data_urban.csv"
)

fp_cases_country <- file.path(
  dir_data_save,
  "case_data_country.csv"
)

###################
#### LOAD DATA ####
###################

df_historic <- read_delim(fp_historic, delim = ";")
df_new <- read_excel(fp_new)
df_urb_classification <- read_csv(fp_urban_class) %>%
  transmute(
    adm3_pcode = str_replace(ADM3_PCODE, "MG", "MDG"),
    urban = urban_area_weighted_13
  )

df <- bind_rows(df_historic, df_new) %>%
  arrange(
    Year,
    Week,
    District,
    Commune
  ) %>%
  filter(
    str_detect(Cases_Class, "CONF|PROB") # remove suspected cases
  ) %>%
  select(
    adm3_pcode = mdg_com_code,
    year = Year,
    week = Week,
    clinical_form = Clinical_Form,
    cases = Cases_Number
  ) %>%
  complete( # fill in all cases and types for years and areas present in data
    adm3_pcode,
    year = 2012:2022,
    week = 1:53,
    clinical_form = c("PB", "PP", "NP"),
    fill = list(
      cases = 0
    )
  ) %>%
  filter( # remove week 53 if not available and filter to April 1 2022
    year < 2022 | week < 14
  ) %>%
  mutate( # add in date data
    date = ISOweek2date(paste0(year, "-W", str_pad(week, 2, pad = "0"), "-1")),
    .before = year
  ) %>%
  group_by(
    adm3_pcode,
    date,
    clinical_form
  ) %>%
  summarize( # date for ISO week 53 of previous year is same as week 1 of next
    year = max(year),
    week = min(week),
    cases = sum(cases),
    .groups = "drop"
  ) %>%
  left_join(
    df_urb_classification,
    by = "adm3_pcode"
  ) %>%
  select(
    adm3_pcode,
    date,
    year,
    week,
    clinical_form,
    cases,
    urban
  )

####################
#### URBAN DATA ####
####################

# filter the data to the cases and areas we are interested in
# only use pneumonic cases
df_urb <- df %>%
  filter(
    urban,
    str_length(adm3_pcode) == 11, # remove adm2 codes
    clinical_form == "PP"
  ) %>%
  group_by(
    date,
    year,
    week
  ) %>%
  summarize(
    cases = sum(cases),
    .groups = "drop"
  )

######################
#### COUNTRY DATA ####
######################

df_country <- df %>%
  group_by(
    date,
    year,
    week
  ) %>%
  summarize(
    cases = sum(cases),
    .groups = "drop"
  ) %>%
  mutate(
    cases_3_weeks = rollsumr(cases, 3, fill = NA)
  )

###################
#### SAVE DATA ####
###################

write_csv(df, fp_cases_detailed)
write_csv(df_urb, fp_cases_urban)
write_csv(df_country, fp_cases_country)
