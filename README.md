# Copyright (c) 2023 Mike Dereviankin

This repository contains code that is available under a MIT license.

# General MICE Imputation Workflow for NHANES 2003-2004 PCB Human Serum Data

## Overview

This repository contains an R script that demonstrates a workflow for imputing missing at random (MAR) values found in the NHANES 2003-2004 PCB human serum data. The imputation is performed using the Multiple Imputation by Chained Equations (MICE) method, taking into account applicable demographical, cholesterol, and body measurement covariates.

## Background on MICE

Multiple Imputation by Chained Equations (MICE) is a statistical technique used to handle missing data. Instead of filling in a single value for each missing data point, MICE fills in the missing data multiple times, creating multiple "complete" datasets. This approach accounts for the uncertainty around the true value of the missing data. The imputed datasets can then be analyzed separately, and the results can be pooled to get a single, more accurate and robust estimate.

In this workflow, the MICE method is applied using the `mice` package in R. The method used for imputation is "pmm" (predictive mean matching), which is a semi-parametric method that works well for various types of data.

## Code Structure

1. **Loading Libraries**: Essential libraries for data manipulation, visualization, and imputation are loaded.
2. **Data Preparation**: The dataset is loaded, and necessary data transformations, including converting certain columns to factors, are performed.
3. **MICE Imputation**: The MICE algorithm is applied to the dataset to impute missing values.
4. **Visualization**: The actual vs. imputed values are visualized using ggplot2 to assess the quality of the imputation.
5. **UMAP Visualization**: UMAP (Uniform Manifold Approximation and Projection) is applied to the imputed dataset to visualize the data in reduced dimensions.

## Usage

1. Clone this repository to your local machine.
2. Ensure you have the required R packages installed.
3. Load your dataset in place of `data.csv`.
4. Run the R script.

## Author

M. Dereviankin

## Date

2-Nov-2023
