library(tidyverse)
library(dplyr)
library(dplyr)

raw_data <- read.csv("Life Expectancy Data.csv", header = TRUE, sep = ",")

raw_data <- raw_data %>% filter(Year==2015) %>%
  # 1. Calculate NA count (if you haven't already in the 'raw' stage)
  mutate(na_count = rowSums(is.na(.))) %>%
  
  # 2. Sort so the rows with the FEWEST NAs are at the top
  arrange(na_count) %>%
  
  # 3. Keep the unique rows based on name (keeps the top one, which is now the best one)
  distinct(Country, .keep_all = TRUE)


# 1. Define the static list of columns to drop (keeps the pipeline clean)
cols_to_drop <- c("Year", "Alcohol", "Hepatitis.B", "Measles", "BMI",
                  "Total.expenditure", "Diphtheria", "HIV.AIDS", "thinness..1.19.years",
                  "thinness.5.9.years", "na_count", "percentage.expenditure"
)

clean_data <- raw_data %>%
  # 2. Filter early: Don't process rows you are going to throw away
  filter(Population >= 100000) %>%
  select(-all_of(cols_to_drop)) %>% mutate(na_count = rowSums(is.na(.)))

#We will drop the two countries with GDP or BMI NA
clean_data <- clean_data %>% filter(Country != "Papua New Guinea") %>% filter(Country != "Syrian Arab Republic")

#Check for NAs
colSums(is.na(clean_data)) 

# 1. Calculate the values first
mean_val <- mean(clean_data$Polio, na.rm = TRUE)
median_val <- median(clean_data$Polio, na.rm = TRUE)

# 2. Plot the histogram
hist(clean_data$Polio, breaks = 50, main = "Distribution of Polio", xlab = "Tourists")

# 3. Add the vertical lines
abline(v = mean_val, col = "red", lwd = 2)              # Mean in Red
abline(v = median_val, col = "blue", lwd = 2, lty = 2)  # Median in Blue (dashed)

# 4. Add a legend so you know which is which
legend("topright", legend = c("Mean", "Median"), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

#https://www.who.int/news-room/questions-and-answers/item/herd-immunity-lockdowns-and-covid-19
#Ref for 80% on Polio
#We're turning Polio into binary

clean_data$Polio_binary <- ifelse(clean_data$Polio > 80, 1, 0)
ggplot(clean_data, aes(x = factor(Polio_binary), fill = factor(Polio_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Below 80", "1" = "Above 80")) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "steelblue"), guide = "none") +
  labs(
    title = "Count of Countries by Polio Vaccination Rate",
    subtitle = "Threshold: 80% Vaccination",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()

clean_data$Status_binary <- ifelse(clean_data$Status == "Developed", 1, 0)
ggplot(clean_data, aes(x = factor(Status_binary), fill = factor(Status_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Developing", "1" = "Developed")) +
  scale_fill_manual(values = c("0" = "salmon", "1" = "steelblue"), guide = "none") +
  labs(
    title = "Count of Countries by Development",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()

clean_data <- clean_data %>% 
  mutate(
    schooling_1to5 = cut(
      clean_data$Schooling,
      breaks = 5,
      labels = 1:5,
      include.lowest = TRUE
    )
  )
ggplot(clean_data, aes(x = factor(schooling_1to5), fill = factor(schooling_1to5))) +
  geom_bar() +
  labs(
    title = "Count of Countries by Schooling category",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()


clean_data <- clean_data %>% 
  mutate(
    schooling_1to5 = cut(
      clean_data$Schooling,
      breaks = 5,
      labels = 1:5,
      include.lowest = TRUE
    )
  )
ggplot(clean_data, aes(x = factor(schooling_1to5), fill = factor(schooling_1to5))) +
  geom_bar() +
  labs(
    title = "Count of Countries by Schooling category",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()


#Ref for HDI Categories
#https://hdr.undp.org/reports-and-publications/2020-human-development-report/data-readers-guide
hist(clean_data$Income.composition.of.resources, breaks = 50, main = "Distribution of HDI", xlab = "BMI")
clean_data <- clean_data %>%
  mutate(
    HDICat = case_when(
      Income.composition.of.resources < 0.55 ~ "low",
      Income.composition.of.resources >= 0.55 & Income.composition.of.resources < 0.7 ~ "medium",
      Income.composition.of.resources >= 0.7 & Income.composition.of.resources < 0.8 ~ "high",
      Income.composition.of.resources > 0.8 ~ "very high"
    ),
    # Essential: Convert to factor so R knows "low" < "moderate" < "high"
    # Otherwise, charts will sort them alphabetically (High, Low, Moderate)
    HDICat = factor(HDICat, levels = c("low", "medium", "high", "very high"))
  )
ggplot(clean_data, aes(x = factor(HDICat), fill = factor(HDICat))) +
  geom_bar() +
  labs(
    title = "Count of Countries by HDI category",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()

clean_data <- clean_data %>% select(-Schooling, -Income.composition.of.resources, -na_count)


