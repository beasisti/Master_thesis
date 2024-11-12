# **Bayesian spatio-temporal models for over-threshold ozone pollution in Lombardia**

### Description
This repository contains the code used for my Master's thesis at Politecnico di Milano, titled *Bayesian spatio-temporal models for over-threshold ozone pollution in Lombardia*. The research focuses on modeling the exceedance of ozone concentration thresholds across various locations in Lombardia (Northern Italy), aiming to identify environmental and meteorological factors influencing ozone levels.

### Repository structure
* ```Data\```: contains the processed datasets, including monthly counts of ozone threshold exceedances and relevant meteorological covariates.
*  ```R Code\```: contains R scripts for data preprocessing and exploratory data analysis (EDA).
*  ```Model\```: includes Stan model files for the Bayesian hierarchical models applied to ozone threshold exceedances.

### Project overview  
The analysis involves a Bayesian hierarchical framework to model monthly counts of days exceeding certain ozone concentration thresholds, integrating both spatial and temporal components. Key parts include:
* **Data preprocessing**: cleaning and transforming raw ozone and covariate data.
* **Exploratory data analysis (EDA)**: initial insights and visualizations.
* **Modeling**: spatio-temporal binomial regression models implemented in Stan.

