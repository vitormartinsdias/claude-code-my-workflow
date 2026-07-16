# Cleaning the environment

rm(list = ls())

# Loading packages

library(readxl)
library(writexl)
library(tidyverse)
library(gt)
library(magrittr)
library(vdemdata)
library(wbstats)
library(describedata)
library(modelsummary)
library(kableExtra)
library(survival)
library(survminer)
library(plm)
library(lmtest)
library(sandwich)
library(gtsummary)
library(car)
library(rnaturalearth)
library(sf)
library(fixest)
library(marginaleffects)
library(did)
library(pdftools)

gcb_data <- readRDS("gcb_data.rds")

# Preparing the map

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica")

ngfs_members <- data.frame(
  gu_a3 = c(
    "ALB",
    "ARG",
    "ARM",
    "AUS",
    "AUT",
    "BEL",
    "BRA",
    "KHM",
    "CAN",
    "CHL",
    "CHN",
    "COL",
    "CRI",
    "HRV",
    "CYP",
    "DNK",
    "EGY",
    "EST",
    "FIN",
    "FRA",
    "GEO",
    "DEU",
    "GHA",
    "GRC",
    "GGY",
    "HKG",
    "HUN",
    "ISL",
    "IND",
    "IDN",
    "IRL",
    "IMN",
    "ISR",
    "ITA",
    "JPN",
    "JEY",
    "JOR",
    "LVA",
    "LTU",
    "LUX",
    "MYS",
    "MLT",
    "MUS",
    "MEX",
    "MCO",
    "MAR",
    "NZL",
    "NOR",
    "PRY",
    "PER",
    "PHL",
    "POL",
    "PRT",
    "DOM",
    "ROU",
    "RUS",
    "SEN",
    "SRB",
    "SYC",
    "SGP",
    "SVK",
    "SVN",
    "ZAF",
    "KOR",
    "ESP",
    "SWE",
    "CHE",
    "TZA",
    "THA",
    "TTO",
    "TUN",
    "TUR",
    "UKR",
    "GBR",
    "USA",
    "URY"
  ),
  member = "NGFS Member"
)

world <- world |>
  left_join(ngfs_members, by = c("gu_a3" = "gu_a3")) |>
  mutate(member = ifelse(is.na(member), "Non-member", member))

ngfs_map <- ggplot(data = world) +
  geom_sf(aes(fill = member), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c("NGFS Member" = "darkgrey", "Non-member" = "white")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "World Map by Network for Greening the Financial 
    System (NGFS) Membership",
    fill = "Membership Status"
  )

ggsave(
  plot = ngfs_map,
  filename = "ngfs_map.png",
  bg = "white",
  dpi = 300
)

# Descriptive Statistics

descriptives <- gcb_data

# Country (n=136) and year (n=21)

descriptives %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  print(n = 25)

# Renaming and labeling the data for descriptive statistics purposes
descriptives <- descriptives %>%
  mutate(ngfs_member = ifelse(ngfs_member == 1, "Member", "Non-Member")) %>%
  select(
    ngfs_member,
    "Greenhouse gas emissions" = "log_ed_ghg_emissions",
    "Trade dependence" = "log_trade",
    "GDP per capita" = "log_gdp_pc",
    "GDP per capita growth" = "log_gdp_pc_growth_shifted",
    "Natural resources" = "log_tot_nat_resources_shifted",
    "Population growth" = "log_pop_growth_shifted",
    "Population density" = "log_pop_density",
    "Agricultural land" = "sqrt_agric_area",
    "Forest land" = "sqrt_forest_area",
    "Corruption index (0-1)" = "vdem_corruption",
    "Deliberative democracy index (0-1)" = "vdem_delibdem",
    "Rule of law index" = "vdem_rulelaw", # unreliable measure better captured by V-Dem's Rigorous and impartial public administration variable due to the autonomy of central banks, i.e., state capacity
    "State capacity index (0-1)" = "vdem_statecapacity_scaled",
    "Region" = "cat_region"
  )

# Creating the survival data

sgcb_data <- gcb_data %>%
  mutate(time = year, event = ngfs_member)

# Right-censoring the data to drop countries for the years after they joined the NGFS so that I do not overcount

sgcb_data <- sgcb_data %>%
  arrange(country, year) %>%
  group_by(country) %>%
  slice(if (1 %in% event) seq(match(1, event)) else row_number()) %>%
  ungroup

# Saving the survival data
write_xlsx(sgcb_data, "sgcb_data.xlsx")

kmsurvivalinc <- survfit(
  Surv(sgcb_data$time, sgcb_data$event) ~ sgcb_data$dincome_level
)

center = theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsurvplot(
  kmsurvivalinc,
  data = sgcb_data,
  break.time.by = 1,
  xlim = c(2015, 2021),
  legend.title = "",
  legend.labs = c("Middle and Low Income", "Upper and High Income"),
  legend = "bottom",
  ggtheme = center,
  font.title = c(10, "bold"),
  font.x = c(10),
  font.y = c(10),
  linetype = c("solid", "dashed"),
  palette = c("black", "gray"),
  fun = "event",
  conf.int = TRUE, # Show confidence intervals for the survival curves
  pval = TRUE, # Show p-value of the log-rank test
  risk.table = FALSE, # Show risk table
  xlab = "Year", # Label for the x-axis
  ylab = "NGFS Membership Probability", # Label for the y-axis
  title = "Probability of Joining the Network for Greening the Financial System by Country-Level Income"
)

# Cox models

coxm1 <- coxph(
  Surv(time, event) ~ log_ed_ghg_emissions,
  # factor(cat_region),
  data = sgcb_data
)

coxm2 <- coxph(
  Surv(time, event) ~
    log_ed_ghg_emissions +
      #      factor(cat_region) +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      log_pop_density +
      sqrt_agric_area +
      sqrt_forest_area,
  data = sgcb_data
)

coxm3 <- coxph(
  Surv(time, event) ~
    log_ed_ghg_emissions +
      #      factor(cat_region) +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #      log_pop_density +
      sqrt_agric_area +
      sqrt_forest_area +
      vdem_corruption +
      vdem_delibdem +
      vdem_statecapacity_scaled,
  data = sgcb_data
)

coxmodels <- list(
  "Baseline" = coxm1,
  "Ecological and Economic Indicators" = coxm2,
  "Political Indicators" = coxm3
)

scoef_map <- c(
  "log_ed_ghg_emissions" = "Greenhouse gas emissions",
  "factor(cat_region)East + South Asia and Pacific" = "East + South Asia and Pacific",
  "factor(cat_region)Latin America and Caribbean" = "Latin America and Caribbean",
  "factor(cat_region)Middle East and Africa" = "Middle East and Africa",
  "factor(cat_region)North America" = "North America",
  "log_trade" = "Trade dependence",
  "log_gdp_pc" = "GDP per capita",
  "log_gdp_pc_growth_shifted" = "GDP per capita growth",
  "log_tot_nat_resources_shifted" = "Natural resources",
  #  "log_pop_density" = "Population density",
  "sqrt_agric_area" = "Agricultural land",
  "sqrt_forest_area" = "Forest land",
  "vdem_corruption" = "Corruption index",
  "vdem_delibdem" = "Deliberative democracy",
  "vdem_statecapacity_scaled" = "Administrative capacity"
)

cox_table <- modelsummary(
  coxmodels,
  stars = TRUE,
  fmt = 2,
  gof_map = c("nobs", "adj.r.squared", "bic"),
  coef_map = scoef_map,
  title = "Cox Regression Estimates Predicting the Probability of NGFS Membership",
  notes = "Source: The data have been collected from the World Bank, the Network for Greening the Financial System, and the Varieties of Democracy (V-Dem) Project. Europe is the reference group for the region variable.",
  align = "lddd",
  output = "gt"
) %>%
  cols_width(
    " " ~ px(200),
    everything() ~ px(135)
  )

cox_table

modelplot(
  coxm3,
  coef_map = rev(scoef_map),
  coef_omit = "Intercept",
  color = "black"
) +
  geom_vline(
    xintercept = 0,
    color = "darkgray",
    linetype = "dashed",
    linewidth = .75
  ) +
  labs(
    title = "Figure 1: Predictors of NGFS Membership",
    caption = "See appendix for data sources."
  )

# PLM Package Results

pfmt_data <- pdata.frame(gcb_data, index = c("country", "year"))

# Two-way fixed effects based on Chesler et al. 2023

twowaym1 <- plm(
  log_ed_ghg_emissions ~
    factor(ngfs_member) +
      factor(cat_region),
  data = pfmt_data,
  model = "within",
  index = c("country", "year"),
  effect = "twoway"
)

twowaym2 <- plm(
  log_ed_ghg_emissions ~
    factor(ngfs_member) +
      factor(cat_region) +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #                log_pop_density +
      log_pop_growth_shifted +
      sqrt_agric_area +
      sqrt_forest_area,
  data = pfmt_data,
  model = "within",
  index = c("country", "year"),
  effect = "twoway"
)

twowaym3 <- plm(
  log_ed_ghg_emissions ~
    factor(ngfs_member) +
      factor(cat_region) +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #                log_pop_density +
      log_pop_growth_shifted +
      sqrt_agric_area +
      sqrt_forest_area +
      vdem_corruption +
      vdem_statecapacity_scaled +
      vdem_delibdem,
  data = pfmt_data,
  model = "within",
  index = c("country", "year"),
  effect = "twoway"
)

twowaymodels <- list(
  "Model 1" = twowaym1,
  "Model 2" = twowaym2,
  "Model 3" = twowaym3
)

# Driscoll-Kraay standard errors with a 5-year lag for each model

dk_se_list <- list(
  "Model 1" = vcovSCC(twowaym1, type = "HC0", max.lag = 5),
  "Model 2" = vcovSCC(twowaym2, type = "HC0", max.lag = 5),
  "Model 3" = vcovSCC(twowaym3, type = "HC0", max.lag = 5)
)

coef_map1 <- c(
  "factor(ngfs_member)1" = "NGFS Member",
  "factor(cat_region)East + South Asia and Pacific" = "East + South Asia and Pacific",
  "factor(cat_region)Latin America and Caribbean" = "Latin America and Caribbean",
  "factor(cat_region)Middle East and Africa" = "Middle East and Africa",
  "factor(cat_region)North America" = "North America",
  "log_trade" = "Trade dependence",
  "log_gdp_pc" = "GDP per capita",
  "log_gdp_pc_growth_shifted" = "GDP per capita growth",
  "log_tot_nat_resources_shifted" = "Natural resources",
  "log_pop_density" = "Population density",
  "log_pop_growth_shifted" = "Population growth",
  "sqrt_agric_area" = "Agricultural land",
  "sqrt_forest_area" = "Forest land",
  "vdem_corruption" = "Corruption index",
  "vdem_statecapacity_scaled" = "State capacity",
  "vdem_delibdem" = "Deliberative democracy"
)

twoway_table <- modelsummary(
  twowaymodels,
  stars = TRUE,
  fmt = 2,
  gof_map = c("nobs", "adj.r.squared", "bic"),
  coef_map = coef_map1,
  vcov = dk_se_list,
  output = "gt"
) %>%
  tab_header(
    title = md(
      "Two-Way Fixed Effects Models Predicting the Log of Greenhouse Gas Emissions (N = 2,856)"
    )
  ) %>%
  tab_source_note(
    source_note = md(
      "The data have been collected from the World Bank, the Network for Greening the Financial System, and the Varieties of Democracy (V-Dem) Project. NGFS non-member and Europe are the reference groups for the NGFS and region variables, respectively."
    )
  ) %>%
  gtsave("plm_twoway_table.png")

vcov_model3 <- vcovSCC(twowaym3, type = "HC0", max.lag = 5)

coefplot <- modelplot(
  coef_map = rev(coef_map1),
  vcov = vcov_model3,
  twowaym3
) +
  theme_minimal() +
  theme(
    text = element_text(size = 18), # Increases the overall text size
    axis.text = element_text(size = 18), # Increases axis text font size
    legend.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 14)
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 0.80
  ) +
  labs(
    title = "Two-way Fixed Effects Model Predicting the Log of Greenhouse 
Gas Emissions (N = 2,856)",
    caption = str_wrap(
      "The model uses Driscoll-Kraay standard errors with a 5-year lag"
    )
  )

ggsave(
  filename = "plm_coefplot.png", # File name
  plot = coefplot, # The plot object to save
  width = 12, # Width of the image in inches
  height = 10, # Height of the image in inches
  dpi = 300, # Resolution in dots per inch
  bg = "white"
)

# Fixest Package Results

gcb_data <- gcb_data %>%
  mutate(ngfs_member = as.factor(ngfs_member))

fixestm1 <- feols(
  log_ed_ghg_emissions ~ ngfs_member | country + year,
  vcov_NW(time = "year", unit = "country", lag = 5),
  data = gcb_data
)

etable(fixestm1)

fixestm2 <- feols(
  log_ed_ghg_emissions ~
    ngfs_member +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #                log_pop_density +
      log_pop_growth_shifted +
      sqrt_agric_area +
      sqrt_forest_area |
      country + year,
  vcov_NW(time = "year", unit = "country", lag = 5),
  data = gcb_data
)

etable(fixestm2)

fixestm3 <- feols(
  log_ed_ghg_emissions ~
    ngfs_member +
      log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #                log_pop_density +
      log_pop_growth_shifted +
      sqrt_agric_area +
      sqrt_forest_area +
      vdem_corruption +
      vdem_statecapacity_scaled +
      vdem_delibdem |
      country + year,
  vcov_NW(time = "year", unit = "country", lag = 5),
  data = gcb_data
)

etable(fixestm3)

fixestmodels <- list(
  "Model 1" = fixestm1,
  "Model 2" = fixestm2,
  "Model 3" = fixestm3
)

# Not working

est_did = feols(
  log_ed_ghg_emissions ~
    log_trade +
      log_gdp_pc +
      log_gdp_pc_growth_shifted +
      log_tot_nat_resources_shifted +
      #                log_pop_density +
      log_pop_growth_shifted +
      sqrt_agric_area +
      sqrt_forest_area +
      vdem_corruption +
      vdem_statecapacity_scaled +
      vdem_delibdem +
      i(year, ngfs_member, 5) |
      country + year,
  vcov_NW(time = "year", unit = "country", lag = 5),
  data = gcb_data
)

rm(list = ls())
