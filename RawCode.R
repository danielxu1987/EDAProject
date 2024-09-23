# EDA Project for CITS4009, compiled by Sifeng Xu, 24525488

# Load  libraries
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gridExtra)
library(knitr)

# load dataset
df <- read.csv("./Countries and death causes.csv", header = T, sep=",")


# Create a sample DataFrame
data <- data.frame(
  A = c(1, -2, 3, 4),
  B = c(-1, 2, -3, 4),
  C = c(5, 6, -7, 0)
)

print("Original DataFrame:")
print(data)

# Logical matrix for negative values
negative_matrix <- data < 0
print("Matrix of Negative Values:")
print(negative_matrix)

# Rows with negative values
rows_with_negatives <- which(rowSums(data < 0) > 0)
print("Rows with Negative Values:")
print(rows_with_negatives)

# Columns with negative values
cols_with_negatives <- which(colSums(data < 0) > 0)
print("Columns with Negative Values:")
print(cols_with_negatives)