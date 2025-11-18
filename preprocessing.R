rm(list = ls())
library(tidyverse)
library(GGally)
library(factoextra)

raw_data <- read.csv("Life Expectancy Data.csv", header = TRUE, sep = ",")

#We will focus only on the data for the year 2015
raw_data <- raw_data %>%
  filter(Year == 2015) %>%
  mutate(na_count = rowSums(is.na(.))) %>%
  arrange(na_count) %>%
  distinct(Country, .keep_all = TRUE)

#Dropping columns that won't be used in analysis
cols_to_drop <- c(
  "Year",
  "Alcohol",
  "Hepatitis.B",
  "Measles",
  "BMI",
  "Total.expenditure",
  "Diphtheria",
  "HIV.AIDS",
  "na_count",
  "percentage.expenditure",
  "infant.deaths"
)
clean_data <- raw_data %>%
  filter(Population >= 100000) %>% #Filtering out small countries
  select(-all_of(cols_to_drop)) %>%
  mutate(na_count = rowSums(is.na(.))) #To check for columns with NA values

#Removing countries that don't have a GDP or Thinness data
clean_data <- clean_data %>%
  filter(Country != "Papua New Guinea") %>%
  filter(Country != "Syrian Arab Republic")
clean_data <- clean_data %>%
  filter(Country != "Sudan") %>%
  filter(Country != "South Sudan")

#Check for NAs
colSums(is.na(clean_data))

#We will combine Thinness data in a single variable by using a simple mean
clean_data <- clean_data %>%
  mutate(
    thinness = (thinness..1.19.years + thinness.5.9.years) / 2
  )
hist(
  clean_data$thinness,
  breaks = 50,
  main = "Distribution of Thinness",
  xlab = "Simple mean of thinness ages 5-9 and 10-19",
  col = "lightblue",
  border = "white"
)

#We will look at Polio to turn it into a binary
#Polio := Polio (Pol3) immunization coverage among 1-year-olds (%)
mean_val <- mean(clean_data$Polio, na.rm = TRUE)
median_val <- median(clean_data$Polio, na.rm = TRUE)

hist(
  clean_data$Polio,
  breaks = 50,
  main = "Distribution of Polio",
  xlab = "Polio immunization coverage among 1-year-olds (%)",
  col = "lightblue",
  border = "white"
)

abline(v = mean_val, col = "red", lwd = 2) #Mean in Red
abline(v = median_val, col = "blue", lwd = 2, lty = 2) #Median in Blue (dashed)
legend(
  "topright",
  legend = c("Mean", "Median"),
  col = c("red", "blue"),
  lwd = 2,
  lty = c(1, 2)
)

#We can see that the mean is around 80%, which coincides with the 80% threshold that WHO sets for herd immunity
#https://www.who.int/news-room/questions-and-answers/item/herd-immunity-lockdowns-and-covid-19/#:~:text=for%20Polio,%20the%20threshold%20is%20about%2080

clean_data$Polio_binary <- ifelse(clean_data$Polio > 80, 1, 0)
#Resulting distribution
ggplot(clean_data, aes(x = factor(Polio_binary), fill = factor(Polio_binary))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Below 80", "1" = "Above 80")) +
  scale_fill_manual(
    values = c("0" = "salmon", "1" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Count of Countries by Polio immunization category",
    subtitle = "Threshold: 80% Polio immunization among 1-year-olds",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()

#We will turn Developed into a 0/1 binary category
clean_data$Status_binary <- ifelse(clean_data$Status == "Developed", 1, 0)
ggplot(
  clean_data,
  aes(x = factor(Status_binary), fill = factor(Status_binary))
) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Developing", "1" = "Developed")) +
  scale_fill_manual(
    values = c("0" = "salmon", "1" = "steelblue"),
    guide = "none"
  ) +
  labs(
    title = "Count of Countries by Development",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()


#We will turn Schooling into a multicategory
mean_val <- mean(clean_data$Schooling, na.rm = TRUE)
median_val <- median(clean_data$Schooling, na.rm = TRUE)

hist(
  clean_data$Schooling,
  breaks = 50,
  main = "Distribution of Schooling",
  xlab = "Schooling",
  col = "lightblue",
  border = "white"
)

abline(v = mean_val, col = "red", lwd = 2) #Mean in Red
abline(v = median_val, col = "blue", lwd = 2, lty = 2) #Median in Blue (dashed)
legend(
  "topright",
  legend = c("Mean", "Median"),
  col = c("red", "blue"),
  lwd = 2,
  lty = c(1, 2)
)
max(clean_data$Schooling) #20.4
min(clean_data$Schooling) #4.9
#That gives us a range of 15.5, we will divide it into 5 categories
clean_data <- clean_data %>%
  mutate(
    schooling_1to5 = cut(
      clean_data$Schooling,
      breaks = 5,
      labels = 1:5,
      include.lowest = TRUE
    )
  )
ggplot(clean_data, aes(x = schooling_1to5, fill = schooling_1to5)) +
  geom_bar() +
  labs(
    title = "Count of Countries by Schooling category",
    x = "Category",
    y = "Number of Countries",
    fill = "Schooling category"
  ) +
  theme_minimal()

#We will categorize the HDI column into the official categories
#https://hdr.undp.org/reports-and-publications/2020-human-development-report/data-readers-guide#:~:text=Human%20development%20classification
hist(
  clean_data$Income.composition.of.resources,
  breaks = 50,
  main = "Distribution of HDI",
  xlab = "BMI",
  col = "lightblue",
  border = "white"
)
clean_data <- clean_data %>%
  mutate(
    HDICat = case_when(
      Income.composition.of.resources < 0.55 ~ "low",
      Income.composition.of.resources >= 0.55 &
        Income.composition.of.resources < 0.7 ~ "medium",
      Income.composition.of.resources >= 0.7 &
        Income.composition.of.resources < 0.8 ~ "high",
      Income.composition.of.resources > 0.8 ~ "very high"
    ),
    HDICat = factor(HDICat, levels = c("low", "medium", "high", "very high"))
  )
ggplot(clean_data, aes(x = HDICat, fill = HDICat)) +
  geom_bar() +
  labs(
    title = "Count of Countries by HDI category",
    x = "Category",
    y = "Number of Countries"
  ) +
  theme_minimal()

#We will remove columns that we transformed
clean_data <- clean_data %>%
  select(
    -Polio,
    -Schooling,
    -Status,
    -Income.composition.of.resources,
    -na_count,
    -thinness..1.19.years,
    -thinness.5.9.years
  )

#Summarize Quantitative columns (will be used in PCA)
quant_columns <- c(
  "Life.expectancy",
  "Adult.Mortality",
  "thinness",
  "under.five.deaths",
  "GDP",
  "Population"
)
summary_table <- clean_data %>%
  select(all_of(quant_columns)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Variable) %>%
  summarise(
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE)
  )
print(summary_table)
ggpairs(clean_data %>% select(all_of(quant_columns)))
