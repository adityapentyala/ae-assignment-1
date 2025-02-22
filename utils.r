library("readxl")
library("writexl")

raw_data <- read.csv("data/data.csv", header=TRUE)
subset_data <- raw_data[, c(1, 2, 23, 34, 37, 38, 41, 93)]
write_xlsx(raw_data, "./data/all_processed_data.xlsx", col_names=TRUE)
write_xlsx(subset_data, "./data/processed_data.xlsx", col_names=TRUE)