## ABSTRACT

In the ever-evolving landscape of the used automotive industry, predictive analysis emerges as a linchpin, irrespective of a business's role as a buyer or seller. The ability to accurately price cars is the key to maximizing profitability, particularly in a market that is exploding globally. In India, the used car market is expected to more than double over the next 5 years as buyers begin to forego going to a dealership for their new vehicle. This report focuses on an exploration of key variables influencing vehicle pricing, leveraging a data set of used cars sold on Car Dekho, which is an online marketplace for used cars. Highlighting the efficacy of regression models in the automotive sector, the study aims to investigate the interplay of variables for accurate predictive analysis.

# Car Price Prediction Analysis

This repository contains R code for conducting an exploratory data analysis (EDA) and building predictive models for car selling prices. The analysis includes data preprocessing, visualization, and the creation of multiple linear regression models to predict car prices.

## Getting Started

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/Team-85/car-price-prediction.git
   cd car-price-prediction
   ```

2. **Install R Dependencies:**
   Open R or RStudio and install the required libraries:
   ```R
   install.packages(c("GGally", "MASS", "dplyr", "lars", "leaps", "corrplot", "pls", "RColorBrewer", "tidyverse", "visdat", "ggplot2"))
   ```

3. **Dataset:**
   Download the "Car details v3.csv" dataset and place it in the root directory of the project.

4. **Run the Code:**
   Open the R script in your preferred R environment and run the code.

## Code Overview

- **Data Loading:**
  The script starts by loading the necessary R libraries and reading the dataset from "Car details v3.csv."

- **Exploratory Data Analysis (EDA):**
  Conducts an EDA, checking for missing values, creating dummy variables for categorical features, and visualizing relationships between variables using scatter plots, box plots, and heatmaps.

- **Data Cleaning:**
  Removes rows with outliers in the selling price and kilometers driven.

- **Model Building:**
  Builds three regression models (Linear, Stepwise, Log-Log) to predict car selling prices. The models are evaluated using mean squared error (MSE) and R-squared.

## Results and Insights

The analysis provides insights into the relationships between car features and selling prices, identifies potential outliers, and evaluates the performance of different regression models.

## Notes

- Make sure to customize the paths and file names according to your environment.
- The dataset used in this analysis is "Car details v3.csv," and the code assumes its presence in the project directory.
