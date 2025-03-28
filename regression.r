library("readxl")
library("dplyr")
library("tidyr")
library("erer")
library("ggplot2")
library("caret")
library("writexl")

raw_dataset <- as.data.frame(read_excel("data/all_processed_data.xlsx"))
raw_dataset <- drop_na(raw_dataset)
raw_dataset <- raw_dataset[order(raw_dataset$Bankrupt., decreasing=TRUE),]
raw_dataset_reduced <- raw_dataset[1:1000,]
set.seed(123)  # Set seed for reproducibility
dataset <- raw_dataset_reduced[sample(nrow(raw_dataset_reduced)), ]

obs <- nrow(raw_dataset)
weights <- ifelse(raw_dataset$Bankrupt.==1, (6819-220)/6819, 220/6819)

train <- dataset[1:800,]
val <- dataset[801:1000,]

formula_1 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL.

formula_2 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL. +
                    Operating.Profit.Per.Share..Yuan...+ ROA.C+ # nolint: line_length_linter.
                    Current.Ratio

formula_3 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Operating.Profit.Per.Share..Yuan...+ ROA.C+ # nolint: line_length_linter.
                    Total.Asset.Turnover + Working.Capital.to.Total.Assets

formula_3_log <- Bankrupt. ~ log(1+Total.debt.Total.net.worth) + log(1+Debt.ratio..) + 
                    log(1+Operating.Profit.Per.Share..Yuan...)+ ROA.C+ # nolint: line_length_linter.
                    log(1+Total.Asset.Turnover) + log(1+Working.Capital.to.Total.Assets)

formula_3_quad <- Bankrupt. ~ I(Total.debt.Total.net.worth^2) + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL. +
                    Operating.Profit.Per.Share..Yuan...+ ROA.C+ # nolint: line_length_linter.
                    Total.Asset.Turnover + Working.Capital.to.Total.Assets


probit_model_base <- glm(formula_1, # nolint
                    data=raw_dataset, family=binomial(link="probit"), x=TRUE)

probit_model_altman <- glm(formula_3, # nolint
                    data=raw_dataset, family=binomial(link="probit"), x=TRUE)

probit_model_altman_log <- glm(formula_3_log, # nolint
                    data=raw_dataset, family=binomial(link="probit"), x=TRUE)

probit_model_altman_quad <- glm(formula_3_quad, # nolint
                    data=raw_dataset, family=binomial(link="probit"), x=TRUE)

weighted_model <- glm(formula_3, data=raw_dataset, family=binomial(link="probit"), x=TRUE, weights=weights)
weighted_model_log <- glm(formula_3_log, data=raw_dataset, family=binomial(link="probit"), x=TRUE, weights=weights)

me_base <- maBina(probit_model_base, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
me_altman <- maBina(probit_model_altman, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
me_altman_log <- maBina(probit_model_altman_log, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
me_altman_quad <- maBina(probit_model_altman_quad, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)

me_weighted_model <- maBina(weighted_model, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
me_weighted_model_log <- maBina(weighted_model_log, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
