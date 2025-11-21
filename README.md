# PCA of WHO Life Expectancy (2010–2015)

Overview
- Purpose: perform exploratory analysis and Principal Component Analysis (PCA) on the WHO life-expectancy dataset to identify main sources of variation between countries and examine relationships with country-level categories (development status, polio coverage, schooling, HDI).
- Data source: Kaggle dataset "Life Expectancy Data" — https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who/data
- Script developed for the Multivariate Analysis course at UC3M Statistics for Data Science 2025-26

Repository structure
- preprocessing.R — data loading, cleaning, feature transforms, basic visualizations and completeness checks
- PCA_outline.R — PCA execution, diagnostics and plots (scree, loadings, contributions, PC scores, biplot, clustering, outlier detection). Note: PCA_outline.R sources preprocessing.R, so running PCA_outline.R will execute preprocessing automatically.
- Life Expectancy Data.csv — input dataset (place in project root)
- README.md — this file

Requirements
- R (tested on 4.5.1)
- R packages: tidyverse, GGally, factoextra, tibble (install with install.packages())

Citation / data license
- Dataset obtained from Kaggle: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who/data  
  Check the dataset page for author and licensing details before redistribution.
