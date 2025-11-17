library(tidyverse)
library(dplyr)

raw <- read.csv("country_data.csv", header = TRUE, sep = ",")

raw <- raw %>%
  select(name, region, population, everything())

raw <- raw %>%
  select(-surface_area, -imports, -employment_services, -employment_industry, -employment_agriculture,
         -forested_area, -exports, -threatened_species, -pop_growth, -pop_density,
         -refugees, -currency, -capital, -iso2)

raw <- raw %>%
  mutate(life_expectancy = rowMeans(across(c(life_expectancy_male, life_expectancy_female)), na.rm = FALSE))

raw <- raw %>%
  mutate(schooling = rowMeans(across(c(secondary_school_enrollment_female, secondary_school_enrollment_male,
                                       primary_school_enrollment_female, primary_school_enrollment_male,
                                       post_secondary_enrollment_female, post_secondary_enrollment_male)), na.rm = FALSE))

raw <- raw %>%
  select(-life_expectancy_male, -life_expectancy_female, -secondary_school_enrollment_female,
         -secondary_school_enrollment_male, -primary_school_enrollment_female, -primary_school_enrollment_male,
         -post_secondary_enrollment_female, -post_secondary_enrollment_male, -gdp, -urban_population_growth)

raw <- raw %>%
  select(name, region, population, na_count, everything())

raw <- raw %>%
  filter(population >= 100)

raw <- raw %>%
  mutate(
    na_count = rowSums(is.na(across(everything())))
  )

hist(raw$schooling)
hist(raw$homicide_rate,breaks = 50)

colSums(is.na(raw))

raw <- raw %>%
  select(-co2_emissions)



