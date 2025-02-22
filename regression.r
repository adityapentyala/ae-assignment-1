library("readxl")
library("dplyr")
library("tidyr")
library("erer")

dataset <- as.data.frame(read_excel("data/all_processed_data.xlsx"))
dataset <- drop_na(dataset)

formula_1 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL.

formula_2 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL. +
                    Operating.Profit.Per.Share..Yuan...+ ROA.C+ # nolint: line_length_linter.
                    Current.Ratio

formula_3 <- Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL. +
                    Operating.Profit.Per.Share..Yuan...+ ROA.C+ # nolint: line_length_linter.
                    Current.Ratio + Total.Asset.Turnover + Working.Capital.to.Total.Assets


probit_model_base <- glm(formula_1, # nolint
                    data=dataset, family=binomial(link="probit"), x=TRUE)

probit_model_altman <- glm(formula_3, # nolint
                    data=dataset, family=binomial(link="probit"), x=TRUE)

me_base <- maBina(probit_model_base, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)
me_altman <- maBina(probit_model_altman, x.mean = TRUE, rev.dum = TRUE, digits = 3, subset.name = NULL, subset.value)