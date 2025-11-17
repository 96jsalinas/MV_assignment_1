library(dplyr)
library(factoextra)
source("preprocessing.R")

#Select just quantitative variables 
pca_data <- clean_data %>%
  select(
    Life.expectancy, Adult.Mortality, thinness,
    under.five.deaths, GDP, Population)

#Check correlation of data and covariance matrix
cor(pca_data)
#Variations in population and GDP are very high, as expected based on the magnitude
#In the PCA we are using "scale. = TRUE" so it won't have an impact
round(cov(pca_data))

#Perform Scaled PCA
pca <- prcomp(pca_data, scale. = TRUE)
pca_named <- get_pca_var(pca)
summary(pca)

#Checking Eigenvalues for Kaiser criterion
pca$sdev^2

#Scree plot
fviz_eig(pca, addlabels = TRUE)

#Based on Eigenvalues > 1 and Scree we choose the first 2 components

#Variable correlation circle (loadings plot)
fviz_pca_var(pca, repel = TRUE)

#PCA in relation to Development Status
fviz_pca_ind(
  pca,
  label = "none",
  habillage = clean_data$Status_binary,
  addEllipses = TRUE,
  repel = TRUE
)

#PCA in relation to Polio
fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$Polio_binary),
  addEllipses = TRUE,
  repel = TRUE
)

#PCA in relation to Schooling
fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$schooling_1to5),
  addEllipses = TRUE,
  repel = TRUE
)

#PCA in relation to HDI
fviz_pca_ind(
  pca,
  label = "none",
  habillage = as.factor(clean_data$HDICat),
  addEllipses = TRUE,
  repel = TRUE
)

#Loadings of features on each component
pca_loadings <- pca$rotation    
pca_loadings
#Scores of each country for each component
pca_scores   <- pca$x         
pca_scores

#Selecting PC1 and PC2
data_with_scores <- cbind(clean_data, pca_scores[, 1:2])

#Variable contributions to PCA
pca_named$contrib
#Contribution of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1)
#Contribution of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2)

#X/Y shows representation in PC1 v. PC2 and gradient represents quality of representation
fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("red", "yellow", "green"))
