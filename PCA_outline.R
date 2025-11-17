library(dplyr)
library(factoextra)
source("preprocessing.R")
pca_data <- clean_data %>%
  select(
    Life.expectancy, Adult.Mortality, thinness,
    under.five.deaths, GDP, Population)

cor(pca_data)
pca <- prcomp(pca_data, scale. = TRUE)

summary(pca)
#Checking variation
pca$sdev^2

fviz_eig(pca, addlabels = TRUE)

fviz_pca_var(pca, repel = TRUE)

fviz_pca_ind(
  pca,
  label = "none",
  habillage = clean_data$Status_binary,
  addEllipses = TRUE,
  repel = TRUE
)

fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$Polio_binary),
  addEllipses = TRUE,
  repel = TRUE
)

fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$schooling_1to5),
  addEllipses = TRUE,
  repel = TRUE
)

fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$HDICat),
  addEllipses = TRUE,
  repel = TRUE
)

pca_loadings <- pca$rotation    # variable loadings
pca_loadings
pca_scores   <- pca$x           # coordinates of countries
pca_scores

data_with_scores <- cbind(clean_data, pca_scores[, 1:2])  # add PC1 & PC2

fviz_contrib(pca, choice = "var", axes = 1)
fviz_contrib(pca, choice = "var", axes = 2)

fviz_pca_biplot(pca, repel = TRUE)

fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("red", "yellow", "green"))

fviz_pca_ind(pca, col.ind = "cos2", gradient.cols = c("red", "yellow", "green"))

fviz_contrib(pca, choice = "var", axes = 1:3)



