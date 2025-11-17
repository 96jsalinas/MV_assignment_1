library(tidyverse)
library(dplyr)

raw_data <- read.csv("country_data.csv", header = TRUE, sep = ",")

raw_data <- raw_data %>%
  select(name, region, population, everything())

raw_data <- raw_data %>%
  select(-surface_area, -imports, -employment_services, -employment_industry, -employment_agriculture,
         -forested_area, -exports, -threatened_species, -pop_growth, -pop_density,
         -refugees, -currency, -capital, -iso2)

raw_data <- raw_data %>%
  mutate(life_expectancy = rowMeans(across(c(life_expectancy_male, life_expectancy_female)), na.rm = FALSE))

raw_data <- raw_data %>%
  mutate(schooling = rowMeans(across(c(secondary_school_enrollment_female, secondary_school_enrollment_male,
                                       primary_school_enrollment_female, primary_school_enrollment_male,
                                       post_secondary_enrollment_female, post_secondary_enrollment_male)), na.rm = FALSE))

raw_data <- raw_data %>%
  select(-life_expectancy_male, -life_expectancy_female, -secondary_school_enrollment_female,
         -secondary_school_enrollment_male, -primary_school_enrollment_female, -primary_school_enrollment_male,
         -post_secondary_enrollment_female, -post_secondary_enrollment_male, -gdp, -urban_population_growth)

raw_data <- raw_data %>%
  filter(population >= 100)

raw_data <- raw_data %>%
  mutate(
    na_count = rowSums(is.na(across(everything())))
  )

raw_data <- raw_data %>%
  select(name, region, population, na_count, everything())

hist(raw_data$schooling)
hist(raw_data$homicide_rate,breaks = 50)

colSums(is.na(raw_data))

raw_data <- raw_data %>%
  select(-co2_emissions)




