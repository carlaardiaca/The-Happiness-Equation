# Load necessary libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(purrr)
library(ggcorrplot)

# Load and initial data cleaning
data_hap <-read.csv ("data/Happiness-Equation.csv")
summary(data_hap)

# 1. Check for missing values
missing_values <- sum(is.na(data_hap))
cat("Number of missing values in dataset: ", missing_values, "\n")

# 2. Summary to get an overview and spot any anomalies
cat("Dataset summary:\n")
summary(data_hap)

# 3. Check for duplicate rows
duplicates <- sum(duplicated(data_hap))
cat("Number of duplicate rows: ", duplicates, "\n")

# 4. Inspect data types for each column
cat("Data types in dataset:\n")
sapply(data_hap, class)

