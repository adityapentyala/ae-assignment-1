library("readxl")
library("dplyr")

dataset <- as.data.frame(read_excel("data/processed_data.xlsx"))
dataset <- drop_na(dataset)

head(dataset)

probit_model <- glm(Bankrupt. ~ Total.debt.Total.net.worth + Debt.ratio.. + 
                    Borrowing.dependency + Degree.of.Financial.Leverage..DFL.,
                    data=dataset, family=binomial(link="probit"), x=TRUE)