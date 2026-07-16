# Loading packages

library(readxl)
library(writexl)
library(haven)
library(expss)
library(tidyverse)
library(janitor)
library(vdemdata)
library(wbstats)
library(describedata)
library(gt)
library(modelsummary)
library(kableExtra)
library(survival)
library(survminer)
library(plm)
library(gtsummary)
library(fixest)
library(margins)
library(did)
library(countrycode)

# Creating the dataset

rm(list = ls())

# Starting with EDGAR emissions data

edgar_ghg_2013_2023 <- read_excel(
  "EDGAR_AR5_GHG_1970_2024.xlsx",
  sheet = "TOTALS BY COUNTRY",
  skip = 9
)

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  clean_names()

# Renaming vars for merging with WB data later

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  rename(country = name, iso3c = country_code_a3)

# Restricting the data for time period

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  select(country, iso3c, y_2013:y_2023)

# Reshaping from wide to long

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    values_to = "ed_ghg_emissions"
  )

# Transforming year

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  mutate(
    year = as.integer(str_remove(year, "y_"))
  )

# Dropping aviation and shipping, which are not countries

edgar_ghg_2013_2023 <- edgar_ghg_2013_2023 |>
  filter(iso3c != "AIR" & iso3c != "SEA")

edgar_clean_data <- edgar_ghg_2013_2023

# Saving the EDGAR cleaned data

write_xlsx(edgar_clean_data, "edgar_clean_data.xlsx")

saveRDS(edgar_clean_data, "edgar_clean_data.rds")

# Cleaning the environment to work with VDEM data

rm(list = ls())

# Importing V-Dem data

vdemdata::vdem -> vdem
vdemdata::codebook -> vdem_codebook

# Keeping only years between 2013 and 2023 and the following vars.:
# v2x_corr (Political Corruption Index)
# v2x_polyarchy (Electoral Democracy Index)
# v2x_libdem (Liberal Democracy Index)
# v2x_partipdem (Participatory Democracy Index)
# v2x_delibdem (Deliberative Democracy Index)
# v2x_egaldem (Egalitarian Democracy Index)
# v2x_rule (Rule of Law)
# year (Year)
# country_name (Country)
# country_text_id (Country ISO3C code)

vdem_clean_data <- vdem |>
  filter(year >= 2013 & year <= 2023) |>
  select(
    country_name,
    country_text_id,
    year,
    v2x_corr,
    v2clrspct,
    v2x_polyarchy,
    v2x_libdem,
    v2x_partipdem,
    v2x_delibdem,
    v2x_egaldem,
    v2x_rule
  )

# Renaming the vars. for merging and easy identification.
# State capacity is based on https://link.springer.com/article/10.1007/s11135-022-01466-x
vdem_clean_data <- vdem_clean_data |>
  rename(
    country = country_name,
    iso3c = country_text_id,
    vdem_corruption = v2x_corr,
    vdem_statecapacity = v2clrspct,
    vdem_electdem = v2x_polyarchy,
    vdem_libdem = v2x_libdem,
    vdem_partipdem = v2x_partipdem,
    vdem_delibdem = v2x_delibdem,
    vdem_egaldem = v2x_egaldem,
    vdem_rulelaw = v2x_rule
  )

# Ensuring year reads as integer

vdem_clean_data <- vdem_clean_data |>
  mutate(year = as.integer(year))

write_xlsx(vdem_clean_data, "vdem_clean_data.xlsx")

saveRDS(vdem_clean_data, "vdem_clean_data.rds")

# Cleaning the environment to work with NGFS data

rm(list = ls())

# Importing the data shared by NGFS

ngfs_clean_data <- read_excel("ngfs_members_date.xlsx")

# Dropping observers and keeping only countries

ngfs_clean_data <- ngfs_clean_data |>
  filter(member_status != "Observer")

# Keeping only observations until 2023
ngfs_clean_data <- ngfs_clean_data |>
  filter(year <= 2023)

# Drop rows for "Abu Dhabi", "Dubai", which work as UAE and "European Union"
ngfs_clean_data <- ngfs_clean_data |>
  filter(!country %in% c("Abu Dhabi", "Dubai", "European Union"))

# Arranging the rows alphabetically based on the country variable
ngfs_clean_data <- ngfs_clean_data |>
  arrange(country)

# Keeping only the earliest membership year for each country

ngfs_clean_data <- ngfs_clean_data |>
  group_by(country) |>
  filter(year == min(year)) |>
  ungroup() |>
  distinct(country, .keep_all = TRUE)

# Creating a an iso3c variable for merging with other datasets

ngfs_clean_data <- ngfs_clean_data |>
  mutate(
    year = as.integer(year),
    iso3c = countrycode(
      sourcevar = country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

# Saving the NGFS clean data

write_xlsx(ngfs_clean_data, "ngfs_clean_data.xlsx")

saveRDS(ngfs_clean_data, "ngfs_clean_data.rds")

# Cleaning the environment to work with World Bank data

rm(list = ls())

# # Importing World Bank data for merging

# # Increasing the default timeout limit

# options(timeout = 120)

# # Setting httr to wait longer
# httr::set_config(httr::timeout(120))

# my_indicators <- c(
#   "gdp_pc" = "NY.GDP.PCAP.KD",
#   "gdp_pc_growth" = "NY.GDP.PCAP.KD.ZG",
#   "pop_total" = "SP.POP.TOTL",
#   "pop_growth" = "SP.POP.GROW",
#   "pop_density" = "EN.POP.DNST",
#   "tot_nat_resources" = "NY.GDP.TOTL.RT.ZS",
#   "forest_area" = "AG.LND.FRST.ZS",
#   "agric_area" = "AG.LND.AGRI.ZS",
#   "trade" = "NE.TRD.GNFS.ZS"
# )

# wb_data <- wb_data(my_indicators, start_date = 2013, end_date = 2023)

# Saving to avoid downloading every time

# write_xlsx(wb_data, "wb_data.xlsx")

# saveRDS(wb_data, "wb_data.rds")

# Loading wb_data

wb_data <- readRDS("wb_data.rds")

# Renaming the year variable

wb_data <- wb_data |>
  rename(year = date) |>
  mutate(year = as.integer(year))

# Generating ngfs_member = 0

wb_data <- wb_data |>
  mutate(ngfs_member = 0)

# Loading the other dataframes for merging with WB

edgar_clean_data <- readRDS("edgar_clean_data.rds")
ngfs_clean_data <- readRDS("ngfs_clean_data.rds")
vdem_clean_data <- readRDS("vdem_clean_data.rds")

# Merge wb_data with ngfs_clean_data using the iso3c variable and renaming the year column
wb_data <- wb_data |>
  left_join(
    ngfs_clean_data |>
      select(iso3c, ngfs_memb_year = year),
    by = "iso3c"
  )

# Checking the merging operation. Only Guernsey and Jersey out.

failed_matches <- ngfs_clean_data |>
  select(iso3c, country) |>
  anti_join(wb_data, by = "iso3c") |>
  distinct()

print(failed_matches)

# Updating ngfs_member to 1 for the countries from the year they became members onward

wb_data <- wb_data |>
  group_by(iso3c) |>
  mutate(
    ngfs_member = ifelse(
      year >= ngfs_memb_year,
      1,
      ngfs_member
    )
  ) |>
  ungroup()

# Not changing membership data for the United Arab Emirates even considering Dubai and Abu Dhabi membership in 2019

# wb_data <- wb_data |>
#   mutate(
#     ngfs_member = if_else(
#       iso3c == "ARE" & year >= 2019,
#       1,
#       ngfs_member
#     )
#   )

# Replacing missing values to 0 for countries that never joined the NGFS

wb_data <- wb_data |>
  mutate(
    ngfs_member = ifelse(
      is.na(ngfs_member),
      0,
      ngfs_member
    )
  )

# Creating a dummy only for members regardless of year

ngfs_iso3c <- unique(ngfs_clean_data$iso3c)

wb_data <- wb_data |>
  mutate(
    dngfs_member = ifelse(
      iso3c %in% ngfs_iso3c,
      1,
      0
    )
  )

# Saving the World Bank cleaned data

wb_clean_data <- wb_data

write_xlsx(wb_clean_data, "wb_clean_data.xlsx")

saveRDS(wb_clean_data, "wb_clean_data.rds")

# Incorporating the income level and region vars by first getting all countries

countries_info <- wb_countries()

# Dropping aggregated regions

countries_info <- countries_info |>
  filter(region != "Aggregates")

# Cleaning to only keep the vars for merging

countries_income <- countries_info[, c(
  "iso3c",
  "income_level",
  "income_level_iso3c",
  "region",
  "region_iso3c"
)]

# Merge income level information with my dataset

wb_clean_data <- wb_clean_data |>
  left_join(countries_income, by = "iso3c")

# Getting the list of unique NGFS members before merging with other datasets
# NGFS members = 92

ngfs_member_countries <- wb_clean_data |>
  filter(ngfs_member == 1) |>
  distinct(country) |>
  arrange(country)

print(ngfs_member_countries, n = 100)

# Merging the datasets with countries that appear across all years and data

gcb_full_data <- wb_clean_data |>
  inner_join(edgar_clean_data, by = c("iso3c", "year")) |>
  inner_join(vdem_clean_data, by = c("iso3c", "year")) |>
  select(-country.y, -country) |> # Drops EDGAR (.y) and V-Dem (country) names
  rename(country = country.x) |> # Keeps World Bank (.x) and renames it back to master country
  arrange(country, year)

# Checking which countries appear only in one or another dataset

before_merging <- wb_clean_data |>
  filter(ngfs_member == 1) |>
  pull(country) |>
  unique()

after_merging <- gcb_full_data |>
  filter(ngfs_member == 1) |>
  pull(country) |>
  unique()

# Checking the difference between the two lists

dropped_countries <- setdiff(before_merging, after_merging)

print(dropped_countries) # "Cayman Islands" "Isle of Man" "Liechtenstein" "Monaco" "Montenegro" "Serbia" are dropped

# Reordering the vars
gcb_full_data <- gcb_full_data |>
  select(
    country,
    year,
    ngfs_member,
    dngfs_member,
    ngfs_memb_year,
    region,
    income_level,
    everything()
  )

# Working with factors and removing redundant variables

gcb_full_data <- gcb_full_data |>
  mutate(
    ngfs_member = as.factor(ngfs_member),
    dngfs_member = as.factor(dngfs_member)
  ) |>
  select(-ngfs_memb_year, -iso2c)

# Saving the merged, manipulated data

write_xlsx(gcb_full_data, "gcb_full_data.xlsx")

saveRDS(gcb_full_data, "gcb_full_data.rds")

# Cleaning the environment to clean gcb_full_data

rm(list = ls())

gcb_full_data <- readRDS("gcb_full_data.rds")

# Cleaning vars using listwise deletion for a preliminary analysis
missvarlist <- c(
  "agric_area",
  "forest_area",
  "trade",
  "gdp_pc",
  "gdp_pc_growth",
  #  "tot_nat_resources", # has 24% of missing data
  "pop_growth",
  "pop_total",
  # "pop_density",
  "vdem_corruption",
  "vdem_statecapacity",
  "vdem_electdem",
  "vdem_libdem",
  "vdem_partipdem",
  "vdem_delibdem",
  "vdem_egaldem",
  "vdem_rulelaw"
)

gcb_data <- gcb_full_data[
  complete.cases(
    gcb_full_data[, missvarlist]
  ),
]

# Keeping if country-year spans 2013-2023
# Dropping countries that appear, e.g., only one year in the data

gcb_data <- gcb_data |>
  group_by(country) |>
  filter(length(unique(year)) == 11) |>
  ungroup()

# After listwise deletion, NGFS members = 80 as opposed to 69 non-members.

ngfs_membership_status <- gcb_data |>
  group_by(dngfs_member) |>
  summarise(num_countries = n_distinct(country))

# Confirming no missing data

missing_datacheck <- gcb_data |>
  summarise(across(everything(), ~ sum(is.na(.))))

# Confirming that the dataset is balanced

country_years_check <- gcb_data |>
  group_by(country) |>
  summarise(num_years = n_distinct(year)) |>
  arrange(num_years)

# Cleaning the environment

rm(list = setdiff(ls(), "gcb_data"))

# Checking the distribution of the continuous vars for transformation

# Those needing log transformation are here
gladder(gcb_data$ed_ghg_emissions)
gladder(gcb_data$trade)
gladder(gcb_data$gdp_pc)
gladder(gcb_data$pop_density)

gcb_data <- gcb_data |>
  mutate(gdp_pc_growth_shifted = gdp_pc_growth - min(gdp_pc_growth) + 1)
gladder(gcb_data$gdp_pc_growth_shifted)

gcb_data <- gcb_data |>
  mutate(pop_growth_shifted = pop_growth - min(pop_growth) + 1)
gladder(gcb_data$pop_growth_shifted)

gcb_data <- gcb_data |>
  mutate(
    tot_nat_resources_shifted = ifelse(
      tot_nat_resources == 0,
      0.1,
      tot_nat_resources
    )
  )
gladder(gcb_data$tot_nat_resources_shifted)

log_vars <- c(
  "ed_ghg_emissions",
  "trade",
  "gdp_pc",
  "pop_growth_shifted",
  "pop_density",
  "gdp_pc_growth_shifted",
  "tot_nat_resources_shifted"
)

gcb_data <- gcb_data |>
  mutate(across(all_of(log_vars), log, .names = "log_{.col}"))

# Those needing square root transformation are here
gladder(gcb_data$agric_area)

gcb_data <- gcb_data |>
  mutate(forest_area_shifted = ifelse(forest_area == 0, 0.1, forest_area))
gladder(gcb_data$forest_area_shifted)

sqrt_vars <- c("agric_area", "forest_area")

gcb_data <- gcb_data |>
  mutate(across(all_of(sqrt_vars), sqrt, .names = "sqrt_{.col}"))

# Checking the categorical independent vars

income_ngfs_tab <- table(
  gcb_data$income_level,
  gcb_data$ngfs_member
)
print(income_ngfs_tab)

# 0 low income countries are NGFS members in the clean dataset
# Recoding income_level as binary to have enough variation
# 0 = high and upper middle income and 1 otherwise

gcb_data$dincome_level <-
  ifelse(
    gcb_data$income_level %in%
      c("High income", "Upper middle income"),
    1,
    0
  )

# Checking the variable transformation

dincome_transform <- table(
  gcb_data$income_level,
  gcb_data$dincome_level
)
print(dincome_transform)

# Checking the NGFS membership distribution by income

dincome_dngs_tab <- table(
  gcb_data$dincome_level,
  gcb_data$dngfs_member
)
print(dincome_dngs_tab)

# Checking the distribution by region and NGFS membership

region_ngfs_tab <- table(
  gcb_data$region,
  gcb_data$ngfs_member
)
print(region_ngfs_tab)

# Recoding the cat_region var for South Asia to be part of East Asia & Pacific and Middle East & North Africa to include Sub-Saharan Africa

gcb_data$cat_region <- dplyr::case_when(
  gcb_data$region %in% c("East Asia & Pacific", "South Asia") ~
    "East Asia and Pacific",
  gcb_data$region %in%
    c("Middle East & North Africa", "Sub-Saharan Africa") ~
    "Middle East and North Africa",
  gcb_data$region == "Europe & Central Asia" ~ "Europe and Central Asia",
  gcb_data$region == "Latin America & Caribbean" ~
    "Latin America and Caribbean",
  gcb_data$region == "North America" ~ "North America",
  TRUE ~ as.character(gcb_data$region) # Handle any unexpected cases
)

# Replace remaining '&' with 'and'

gcb_data$cat_region <- gsub(
  "&",
  "and",
  gcb_data$cat_region
)

# Checking membership by cat_region

cat_region_ngfs_tab <- table(
  gcb_data$cat_region,
  gcb_data$ngfs_member
)
print(cat_region_ngfs_tab)

# Relabeling the categories to reflect the transformation

gcb_data <- gcb_data |>
  mutate(
    cat_region = case_when(
      cat_region == "Middle East and North Africa" ~ "Middle East and Africa",
      cat_region == "East Asia and Pacific" ~ "East and South Asia and Pacific",
      TRUE ~ cat_region # Keeps all other categories as they are
    )
  )

# Checking the variable transformation
gcb_data |>
  count(region, cat_region) |>
  pivot_wider(
    names_from = cat_region,
    values_from = n,
    values_fill = list(n = 0)
  )

# Re-scaling the V-Dem state capacity variable to match the 0-1 scale
# Min-Max normalization formula below

min_vdem_statecapacity <- min(gcb_data$vdem_statecapacity, na.rm = TRUE)
max_vdem_statecapacity <- max(gcb_data$vdem_statecapacity, na.rm = TRUE)

gcb_data <- gcb_data |>
  mutate(
    vdem_statecapacity_scaled = (vdem_statecapacity - min_vdem_statecapacity) /
      (max_vdem_statecapacity - min_vdem_statecapacity)
  )

rm(list = setdiff(ls(), "gcb_data"))

gcb_data <- gcb_data |>
  select(
    country,
    year,
    ngfs_member,
    dngfs_member,
    region,
    cat_region,
    income_level,
    dincome_level,
    everything()
  )

gcb_data$dincome_level <- factor(
  ifelse(
    gcb_data$income_level %in% c("High income", "Upper middle income"),
    1,
    0
  ),
  levels = c(0, 1),
  labels = c("Low income", "High income")
)

# Loading CBI data

cbi_data <- read_dta("cbi_2025_websiteGarriga.dta")

cbi_data <- cbi_data |>
  rename(country = cname) |>
  mutate(
    year = as.integer(year),
    iso3c = countrycode(
      sourcevar = country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

# Fixing the failed matches with the ISO3C codes

cbi_data <- cbi_data |>
  mutate(
    iso3c = case_when(
      country == "Serbia/Yugoslavia (Serbia-Montenegro)" ~ "SRB",
      country == "Serbia and Montenegro" ~ "SRB",
      country == "Sudan, South" ~ "SSD",
      country == "Yemen, North/Yemen Arab Rep." ~ "YEM",
      TRUE ~ iso3c # Leaves all the other countrycode matches untouched
    )
  )

# Arranging by country and year for merging

cbi_data <- cbi_data |>
  arrange(country, year)

# Keeping only observations between 2013 and 2023
# and lvau_garriga and lvaw_garriga

cbi_data <- cbi_data |>
  filter(year >= 2013 & year <= 2023) |>
  select(country, year, iso3c, lvau_garriga, lvaw_garriga) |>
  rename(
    cbi_unweighted = lvau_garriga,
    cbi_weighted = lvaw_garriga
  )

# Filtering missing data - no missing data

cbi_data <- cbi_data |>
  filter(
    !is.na(cbi_unweighted),
    !is.na(cbi_weighted)
  )

# Keep only countries with complete data for all years 2013-2023

cbi_data <- cbi_data |>
  group_by(country) |>
  filter(n() == 11) |>
  ungroup()

# Merging with gcb_data

cbi_data <- cbi_data |>
  select(
    #    country,
    year,
    iso3c,
    cbi_unweighted,
    cbi_weighted
  )

# Eswatini in gcb_data is missing from cbi_data
dropped_in_cbi_merge <- gcb_data |>
  select(iso3c, country, dngfs_member) |>
  distinct() |>
  anti_join(cbi_data, by = "iso3c")

print(dropped_in_cbi_merge)

gcb_data <- gcb_data |>
  inner_join(cbi_data, by = c("iso3c", "year"))

# After merging, NGFS members = 80 as opposed to 65 non-members.

ngfs_membership_status <- gcb_data |>
  group_by(dngfs_member) |>
  summarise(num_countries = n_distinct(country))

rm(list = setdiff(ls(), "gcb_data"))

# Saving the restricted dataset after merging with central bank
# independence index and listwise deletion

gcb_clean_data <- gcb_data

write_xlsx(gcb_clean_data, "gcb_clean_data.xlsx")

saveRDS(gcb_clean_data, "gcb_clean_data.rds")

rm(list = setdiff(ls(), "gcb_clean_data"))

# # Descriptives and regression analysis -- delete after the manuscript is completed.

# gcb_data <- readRDS("gcb_data.rds")

# # Descriptive Statistics

# # Relabeling CBI

# gcb_data <- gcb_data |>
#   apply_labels(
#     cbi_unweighted = "Central Bank Independence Index",
#     cbi_weighted = "Central Bank Independence Index (Weighted)"
#   )

# # Filtering the data for years 2015 to 2021
# gcb_data_2015 <- gcb_data |>
#   filter(year >= 2015 & year <= 2021)

# descriptives <- gcb_data_2015

# # Country (n=133) and year (n=5) - N = 931

# descriptives |>
#   group_by(year) |>
#   summarise(n = n()) |>
#   print(n = 25)

# # Renaming and labeling the data for descriptive statistics purposes

# descriptives <- descriptives |>
#   mutate(ngfs_member = ifelse(ngfs_member == 1, "Member", "Non-Member")) |>
#   select(
#     ngfs_member,
#     "Greenhouse gas emissions" = "log_ed_ghg_emissions",
#     "Trade dependence" = "log_trade",
#     "GDP per capita" = "log_gdp_pc",
#     "GDP per capita growth" = "log_gdp_pc_growth_shifted",
#     "Natural resources" = "log_tot_nat_resources_shifted",
#     "Population growth" = "log_pop_growth_shifted",
#     #         "Population density" = "log_pop_density",
#     "Agricultural land" = "sqrt_agric_area",
#     "Forest land" = "sqrt_forest_area",
#     # "Corruption index (0-1)" = "vdem_corruption",
#     "Deliberative democracy index (0-1)" = "vdem_delibdem",
#     "Central Bank Independence Index (Weighted)" = "cbi_weighted",
#     # "Rule of law index" = "vdem_rulelaw",
#     # "State capacity index (0-1)" = "vdem_statecapacity_scaled",
#     # "Central Bank Independence Index" = "cbi_unweighted",
#     "Region" = "cat_region"
#   )

# desc_table <- descriptives |>
#   tbl_summary(
#     by = ngfs_member,
#     statistic = list(
#       all_continuous() ~ "{mean} ({sd})",
#       all_categorical() ~ "{n} ({p}%)"
#     )
#   ) |>
#   add_p() |>
#   bold_p() |>
#   modify_footnote(everything() ~ NA) |>
#   modify_header(label = "**Variable**") |>
#   as_gt() |>
#   tab_header(
#     title = md(
#       "Descriptive Statistics by Network for Greening the Financial System Membership <br> Country x Years between 2010 and 2021 (N = 1,596)"
#     )
#   ) |>
#   tab_source_note(
#     source_note = md(
#       "The values correspond to the mean for continuous
#       variables and the proportion for categorical variables. The standard
#       deviation is shown in parentheses. The data have been collected from the
#       World Bank, Network for Greening the Financial System,
#       Varieties of Democracy (V-Dem) Project, and Quality of Government Project."
#     )
#   ) |>
#   #    text_case_match(
#   #    "Population density (people per sq. km of land area)" ~ "Population density"
#   #    ) |>
#   cols_label(
#     p.value = md("**Differences** <br> p-value")
#   ) |>
#   cols_align_decimal(
#     columns = everything(),
#     dec_mark = ".",
#     locale = NULL
#   ) |>
#   #    tab_options(
#   #    table.width = pct(90),  # Adjusts table width to 90% of slide width
#   #    column_labels.font.size = px(16),  # Adjust column label font size
#   #    table.font.size = px(14)  # Adjusts overall table text size
#   #    ) |>
#   gtsave("desc_table2015.png")

# coef_map <- c(
#   "ngfs_member::1" = "NGFS Member",
#   #  "factor(cat_region)East + South Asia and Pacific" = "East + South Asia and Pacific",
#   #  "factor(cat_region)Latin America and Caribbean" = "Latin America and Caribbean",
#   #  "factor(cat_region)Middle East and Africa" = "Middle East and Africa",
#   #  "factor(cat_region)North America" = "North America",
#   "log_trade" = "Trade dependence",
#   "log_gdp_pc" = "GDP per capita",
#   "log_gdp_pc_growth_shifted" = "GDP per capita growth",
#   "log_tot_nat_resources_shifted" = "Natural resources",
#   #              "log_pop_density" = "Population density",
#   # "log_pop_growth_shifted" = "Population growth",
#   "sqrt_agric_area" = "Agricultural land",
#   "sqrt_forest_area" = "Forest land",
#   #              "vdem_corruption" = "Corruption index",
#   "vdem_statecapacity_scaled" = "State capacity",
#   "vdem_delibdem" = "Deliberative democracy",
#   # "cbi_unweighted" = "Central Bank Independence Index",
#   "cbi_weighted" = "Central Bank\nIndependence Index (Weighted)"
# )

# # Regression models

# feols1 <- feols(
#   log_ed_ghg_emissions ~ i(ngfs_member) | country + year,
#   vcov_NW(unit = "country", time = "year", lag = 3),
#   data = gcb_data_2015
# )

# feols2 <- feols(
#   log_ed_ghg_emissions ~
#     i(ngfs_member) +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 3),
#   data = gcb_data_2015
# )

# feols3 <- feols(
#   log_ed_ghg_emissions ~
#     ngfs_member +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area +
#     vdem_statecapacity_scaled +
#     cbi_weighted +
#     vdem_delibdem |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 3),
#   data = gcb_data_2015
# )

# fetableint <- modelsummary(feols3)

# # List of models

# femodels <- list(
#   "Model 1" = feols1,
#   "Model 2" = feols2,
#   "Model 3" = feols3
# )

# # Regression table

# fetable <- modelsummary(
#   femodels,
#   stars = TRUE,
#   fmt = 2,
#   coef_map = coef_map,
#   gof_map = c(
#     "nobs",
#     "r2.within.adjusted",
#     "bic",
#     "vcov.type",
#     "FE: country",
#     "FE: year"
#   ),
#   #  vcov = dk_se_list,
#   output = "gt"
# ) |>
#   tab_header(
#     title = md(
#       "Two-Way Fixed Effects Models Predicting the Log of Greenhouse Gas Emissions (N = 1,596)"
#     )
#   ) |>
#   tab_source_note(
#     source_note = md(
#       "The data have been collected from the
#       World Bank, Network for Greening the Financial System,
#       Varieties of Democracy (V-Dem) Project,
#       and Quality of Government Project.
#       NGFS non-member is the reference groups for the NGFS variable."
#     )
#   ) |>
#   gtsave("fetable2015.png")

# # Coefficient plot

# coefplot <- modelplot(
#   coef_map = rev(coef_map),
#   # vcov = vcov_model3,
#   feols3
# ) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 18), # Increases the overall text size
#     axis.text = element_text(size = 18), # Increases axis text font size
#     legend.text = element_text(size = 16),
#     plot.title = element_text(hjust = 0.5),
#     plot.caption = element_text(hjust = 0.5, size = 14)
#   ) +
#   geom_vline(
#     xintercept = 0,
#     color = "red",
#     linetype = "dashed",
#     linewidth = 0.80
#   ) +
#   labs(
#     title = "Two-way Fixed Effects Model Predicting the \n Log of Greenhouse
#     Gas Emissions (N = 1,596)",
#     caption = str_wrap(
#       "The model uses Newey-West standard errors with a 5-year lag"
#     )
#   )

# ggsave(
#   filename = "coefplot2015.png", # File name
#   plot = coefplot, # The plot object to save
#   width = 12, # Width of the image in inches
#   height = 10, # Height of the image in inches
#   dpi = 300, # Resolution in dots per inch
#   bg = "white"
# )

# # Dynamic DID for plotting

# didfeols <- feols(
#   log_ed_ghg_emissions ~
#     i(year, dngfs_member, 2016) +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area +
#     cbi_weighted +
#     vdem_statecapacity_scaled +
#     vdem_delibdem |
#     country + year,
#   #  vcov = "Twoway",
#   data = gcb_data_2015
# )

# iplot(
#   didfeols,
#   xlab = "Year",
#   ylab = "Log of GHG emissions",
#   main = "Effect of NGFS Membership on GHG Emissions Compared to 2016" #,
#   #  ci = 0.95
# )

# # Filtering the data for years 2010 to 2021
# gcb_data_2010 <- gcb_data |>
#   filter(year >= 2010 & year <= 2021)

# descriptives <- gcb_data_2010

# # Country (n=133) and year (n=12) - N = 1,596

# descriptives |>
#   group_by(year) |>
#   summarise(n = n()) |>
#   print(n = 25)

# # Renaming and labeling the data for descriptive statistics purposes

# descriptives <- descriptives |>
#   mutate(ngfs_member = ifelse(ngfs_member == 1, "Member", "Non-Member")) |>
#   select(
#     ngfs_member,
#     "Greenhouse gas emissions" = "log_ed_ghg_emissions",
#     "Trade dependence" = "log_trade",
#     "GDP per capita" = "log_gdp_pc",
#     "GDP per capita growth" = "log_gdp_pc_growth_shifted",
#     "Natural resources" = "log_tot_nat_resources_shifted",
#     "Population growth" = "log_pop_growth_shifted",
#     #         "Population density" = "log_pop_density",
#     "Agricultural land" = "sqrt_agric_area",
#     "Forest land" = "sqrt_forest_area",
#     # "Corruption index (0-1)" = "vdem_corruption",
#     "Deliberative democracy index (0-1)" = "vdem_delibdem",
#     "Central Bank Independence Index (Weighted)" = "cbi_weighted",
#     # "Rule of law index" = "vdem_rulelaw",
#     # "State capacity index (0-1)" = "vdem_statecapacity_scaled",
#     # "Central Bank Independence Index" = "cbi_unweighted",
#     "Region" = "cat_region"
#   )

# desc_table <- descriptives |>
#   tbl_summary(
#     by = ngfs_member,
#     statistic = list(
#       all_continuous() ~ "{mean} ({sd})",
#       all_categorical() ~ "{n} ({p}%)"
#     )
#   ) |>
#   add_p() |>
#   bold_p() |>
#   modify_footnote(everything() ~ NA) |>
#   modify_header(label = "**Variable**") |>
#   as_gt() |>
#   tab_header(
#     title = md(
#       "Descriptive Statistics by Network for Greening the Financial System Membership <br> Country x Years between 2010 and 2021 (N = 1,596)"
#     )
#   ) |>
#   tab_source_note(
#     source_note = md(
#       "The values correspond to the mean for continuous
#       variables and the proportion for categorical variables. The standard
#       deviation is shown in parentheses. The data have been collected from the
#       World Bank, Network for Greening the Financial System,
#       Varieties of Democracy (V-Dem) Project, and Quality of Government Project."
#     )
#   ) |>
#   #    text_case_match(
#   #    "Population density (people per sq. km of land area)" ~ "Population density"
#   #    ) |>
#   cols_label(
#     p.value = md("**Differences** <br> p-value")
#   ) |>
#   cols_align_decimal(
#     columns = everything(),
#     dec_mark = ".",
#     locale = NULL
#   ) |>
#   #    tab_options(
#   #    table.width = pct(90),  # Adjusts table width to 90% of slide width
#   #    column_labels.font.size = px(16),  # Adjust column label font size
#   #    table.font.size = px(14)  # Adjusts overall table text size
#   #    ) |>
#   gtsave("desc_table.png")

# coef_map <- c(
#   "ngfs_member::1" = "NGFS Member",
#   #  "factor(cat_region)East + South Asia and Pacific" = "East + South Asia and Pacific",
#   #  "factor(cat_region)Latin America and Caribbean" = "Latin America and Caribbean",
#   #  "factor(cat_region)Middle East and Africa" = "Middle East and Africa",
#   #  "factor(cat_region)North America" = "North America",
#   "log_trade" = "Trade dependence",
#   "log_gdp_pc" = "GDP per capita",
#   "log_gdp_pc_growth_shifted" = "GDP per capita growth",
#   "log_tot_nat_resources_shifted" = "Natural resources",
#   #              "log_pop_density" = "Population density",
#   "log_pop_growth_shifted" = "Population growth",
#   "sqrt_agric_area" = "Agricultural land",
#   "sqrt_forest_area" = "Forest land",
#   #              "vdem_corruption" = "Corruption index",
#   # "vdem_statecapacity_scaled" = "State capacity",
#   "vdem_delibdem" = "Deliberative democracy",
#   # "cbi_unweighted" = "Central Bank Independence Index",
#   "cbi_weighted" = "Central Bank\nIndependence Index (Weighted)"
# )

# # Regression models

# feols1 <- feols(
#   log_ed_ghg_emissions ~ i(ngfs_member) | country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data_2010
# )

# feols2 <- feols(
#   log_ed_ghg_emissions ~
#     i(ngfs_member) +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data_2010
# )

# feols3 <- feols(
#   log_ed_ghg_emissions ~
#     i(ngfs_member) +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area +
#     cbi_weighted +
#     vdem_delibdem |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data_2010
# )

# # List of models

# femodels <- list(
#   "Model 1" = feols1,
#   "Model 2" = feols2,
#   "Model 3" = feols3
# )

# # Regression table

# fetable <- modelsummary(
#   femodels,
#   stars = TRUE,
#   fmt = 2,
#   coef_map = coef_map,
#   gof_map = c(
#     "nobs",
#     "r2.within.adjusted",
#     "bic",
#     "vcov.type",
#     "FE: country",
#     "FE: year"
#   ),
#   #  vcov = dk_se_list,
#   output = "gt"
# ) |>
#   tab_header(
#     title = md(
#       "Two-Way Fixed Effects Models Predicting the Log of Greenhouse Gas Emissions (N = 1,596)"
#     )
#   ) |>
#   tab_source_note(
#     source_note = md(
#       "The data have been collected from the
#       World Bank, Network for Greening the Financial System,
#       Varieties of Democracy (V-Dem) Project,
#       and Quality of Government Project.
#       NGFS non-member is the reference groups for the NGFS variable."
#     )
#   ) |>
#   gtsave("fetable.png")

# # Coefficient plot

# coefplot <- modelplot(
#   coef_map = rev(coef_map),
#   # vcov = vcov_model3,
#   feols3
# ) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 18), # Increases the overall text size
#     axis.text = element_text(size = 18), # Increases axis text font size
#     legend.text = element_text(size = 16),
#     plot.title = element_text(hjust = 0.5),
#     plot.caption = element_text(hjust = 0.5, size = 14)
#   ) +
#   geom_vline(
#     xintercept = 0,
#     color = "red",
#     linetype = "dashed",
#     linewidth = 0.80
#   ) +
#   labs(
#     title = "Two-way Fixed Effects Model Predicting the \n Log of Greenhouse
#     Gas Emissions (N = 1,596)",
#     caption = str_wrap(
#       "The model uses Newey-West standard errors with a 5-year lag"
#     )
#   )

# ggsave(
#   filename = "coefplot.png", # File name
#   plot = coefplot, # The plot object to save
#   width = 12, # Width of the image in inches
#   height = 10, # Height of the image in inches
#   dpi = 300, # Resolution in dots per inch
#   bg = "white"
# )

# # Dynamic DID for plotting

# didfeols <- feols(
#   log_ed_ghg_emissions ~
#     i(year, dngfs_member, 2016) +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area |
#     country + year,
#   #  vcov = "Twoway",
#   data = gcb_data_2010
# )

# iplot(
#   dydid,
#   xlab = "Year",
#   ylab = "Log of GHG emissions",
#   main = "Effect of NGFS Membership on GHG Emissions Compared to 2016" #,
#   #  ci = 0.95
# )

# # Correlation matrix

# cormat <- datasummary_correlation(
#   descriptives,
#   output = "gt"
# ) |>
#   gtsave("cormat.png")

# # Robustness checks

# # Lagged variables and NW standard errors

# library(dplyr)

# test <- gcb_data |>
#   group_by(country) |>
#   mutate(lag1 = lag(log_ed_ghg_emissions, n = 3, default = NULL)) |>
#   ungroup()

# gcb_data_2010 <- gcb_data_2010 |>
#   arrange(country, year) |>
#   group_by(country) |>
#   mutate(
#     lag_log_ed_ghg_emissions = lag(log_ed_ghg_emissions, n = 1, default = NA),
#     lag_log_trade = lag(log_trade, 3),
#     lag_log_gdp_pc = lag(log_gdp_pc, 3),
#     lag_log_gdp_pc_growth_shifted = lag(log_gdp_pc_growth_shifted, 3),
#     lag_log_tot_nat_resources_shifted = lag(log_tot_nat_resources_shifted, 3),
#     lag_log_pop_growth_shifted = lag(log_pop_growth_shifted, 3),
#     lag_sqrt_agric_area = lag(sqrt_agric_area, 3),
#     lag_sqrt_forest_area = lag(sqrt_forest_area, 3),
#     lag_cbi_weighted = lag(cbi_weighted, 3),
#     lag_vdem_delibdem = lag(vdem_delibdem, 3)
#   ) |>
#   ungroup()

# # Two-way fixed effects regression

# feolsrc1 <- feols(
#   log_ed_ghg_emissions ~
#     ngfs_member |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data
# )

# feolsrc2 <- feols(
#   log_ed_ghg_emissions ~
#     ngfs_member +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     #                log_pop_density +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data
# )

# feolsrc3 <- feols(
#   log_ed_ghg_emissions ~
#     ngfs_member +
#     log_trade +
#     log_gdp_pc +
#     log_gdp_pc_growth_shifted +
#     log_tot_nat_resources_shifted +
#     #                log_pop_density +
#     log_pop_growth_shifted +
#     sqrt_agric_area +
#     sqrt_forest_area +
#     #      vdem_corruption +
#     cbi_weighted +
#     #      vdem_statecapacity_scaled +
#     vdem_delibdem |
#     country + year,
#   vcov_NW(unit = "country", time = "year", lag = 5),
#   data = gcb_data
# )

# femodelsrc <- list(
#   "Model 1" = feolsrc1,
#   "Model 2" = feolsrc2,
#   "Model 3" = feolsrc3
# )

# fetablerc <- modelsummary(
#   femodels,
#   stars = TRUE,
#   fmt = 2,
#   #  gof_omit = "aic|r.squared|adj.r.squared|re.within|rmse",
#   gof_map = c(
#     "nobs",
#     "r2.within.adjusted",
#     "bic",
#     "vcov.type",
#     "FE: country",
#     "FE: year"
#   ),
#   #  coef_map = coef_map,
#   #  vcov = dk_se_list,
#   output = "gt"
# ) |>
#   tab_header(
#     title = md(
#       "Two-Way Fixed Effects Models Predicting the Log of Greenhouse Gas Emissions (N = 2,856)"
#     )
#   ) |>
#   tab_source_note(
#     source_note = md(
#       "The data have been collected from the World Bank, the Network for Greening the Financial System, and the Varieties of Democracy (V-Dem) Project. NGFS non-member and Europe are the reference groups for the NGFS and region variables, respectively."
#     )
#   ) |>
#   gtsave("fetable.png")

# coefplot <- modelplot(
#   coef_map = rev(coef_map1),
#   vcov = vcov_model3,
#   twowaym3
# ) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 18), # Increases the overall text size
#     axis.text = element_text(size = 18), # Increases axis text font size
#     legend.text = element_text(size = 16),
#     plot.title = element_text(hjust = 0.5),
#     plot.caption = element_text(hjust = 0.5, size = 14)
#   ) +
#   geom_vline(
#     xintercept = 0,
#     color = "red",
#     linetype = "dashed",
#     linewidth = 0.80
#   ) +
#   labs(
#     title = "Two-way Fixed Effects Model Predicting the Log of Greenhouse
#     Gas Emissions (N = 2,856)",
#     caption = str_wrap(
#       "The model uses Driscoll-Kraay standard errors with a 5-year lag"
#     )
#   )

# ggsave(
#   filename = "coefplot.png", # File name
#   plot = coefplot, # The plot object to save
#   width = 12, # Width of the image in inches
#   height = 10, # Height of the image in inches
#   dpi = 300, # Resolution in dots per inch
#   bg = "white"
# )

# Housekeeping with old data and code

# Methane

# edgar_ch4_2001_2021 <- read_excel("EDGAR_CH4_1970_2022.xlsx")

# edgar_ch4_2001_2021 <- edgar_ch4_2001_2021 |>
#   clean_names()

# # Renaming vars for merging with WB data later

# edgar_ch4_2001_2021 <- edgar_ch4_2001_2021 |>
#   rename(country = name, iso3c = country_code_a3)

# # Restricting the data for time period

# edgar_ch4_2001_2021 <- edgar_ch4_2001_2021 |>
#   select(country, iso3c, y_2001:y_2021)

# # Reshaping from wide to long

# edgar_ch4_2001_2021 <- edgar_ch4_2001_2021 |>
#   pivot_longer(
#     cols = starts_with("y"),
#     names_to = "year",
#     values_to = "ed_ch4_emissions"
#   )

# # Transforming year

# edgar_ch4_2001_2021 <- edgar_ch4_2001_2021 |>
#   mutate(year = as.integer(str_remove(year, "y_")))

# # Repeating the steps above for CO2

# edgar_co2_2001_2021 <- read_excel("EDGAR_IEA_CO2_1970_2022.xlsx")

# edgar_co2_2001_2021 <- edgar_co2_2001_2021 |>
#   clean_names()

# # Renaming vars for merging with WB data later

# edgar_co2_2001_2021 <- edgar_co2_2001_2021 |>
#   rename(country = name, iso3c = country_code_a3)

# # Restricting the data for time period

# edgar_co2_2001_2021 <- edgar_co2_2001_2021 |>
#   select(country, iso3c, y_2001:y_2021)

# # Reshaping from wide to long

# edgar_co2_2001_2021 <- edgar_co2_2001_2021 |>
#   pivot_longer(
#     cols = starts_with("y"),
#     names_to = "year",
#     values_to = "ed_co2_emissions"
#   )

# # Transforming year

# edgar_co2_2001_2021 <- edgar_co2_2001_2021 |>
#   mutate(year = as.integer(str_remove(year, "y_")))

# # Repeating the steps above for N2O

# edgar_n2o_2001_2021 <- read_excel("EDGAR_N2O_1970_2022.xlsx")

# edgar_n2o_2001_2021 <- edgar_n2o_2001_2021 |>
#   clean_names()

# # Renaming vars for merging with WB data later

# edgar_n2o_2001_2021 <- edgar_n2o_2001_2021 |>
#   rename(country = name, iso3c = country_code_a3)

# # Restricting the data for time period

# edgar_n2o_2001_2021 <- edgar_n2o_2001_2021 |>
#   select(country, iso3c, y_2001:y_2021)

# # Reshaping from wide to long

# edgar_n2o_2001_2021 <- edgar_n2o_2001_2021 |>
#   pivot_longer(
#     cols = starts_with("y"),
#     names_to = "year",
#     values_to = "ed_n2o_emissions"
#   )

# # Transforming year

# edgar_n2o_2001_2021 <- edgar_n2o_2001_2021 |>
#   mutate(year = as.integer(str_remove(year, "y_")))

# Merging the EDGAR data

# edgar_clean_data <- merge(
#   edgar_n2o_2001_2021,
#   edgar_ghg_2013_2023,
#   by = c("country", "iso3c", "year"),
#   all = TRUE
# )

# edgar_clean_data <- merge(
#   edgar_clean_data,
#   edgar_co2_2001_2021,
#   by = c("country", "iso3c", "year"),
#   all = TRUE
# )

# edgar_clean_data <- merge(
#   edgar_clean_data,
#   edgar_ch4_2001_2021,
#   by = c("country", "iso3c", "year"),
#   all = TRUE
# )

# # Checking the discrepancies in edgar_n2o_2001_2021

# diff_obs <- anti_join(edgar_clean_data, edgar_n2o_2001_2021, by = "iso3c")
# print(diff_obs)

# # Dropping Marshall and Northern Mariana Islands due to missing data
# # Dropping aviation and shipping, which are not countries
# edgar_clean_data <- edgar_clean_data |>
#   filter(iso3c != "AIR" & iso3c != "SEA" & iso3c != "MHL" & iso3c != "MNP")
