library("readxl")
library("dplyr")
library("tidyr")
library("erer")
library("ggplot2")
library("caret")
library("writexl")

raw_dataset <- as.data.frame(read_excel("data/all_processed_data.xlsx"))
raw_dataset <- drop_na(raw_dataset)

data <- data.frame(raw_dataset$"Total.debt.Total.net.worth" , 
        raw_dataset$"Debt.ratio.." ,
        #raw_dataset$"Borrowing.dependency" ,
        #raw_dataset$"Degree.of.Financial.Leverage..DFL." ,
        raw_dataset$"Operating.Profit.Per.Share..Yuan..." , 
        raw_dataset$"ROA.C" ,
        raw_dataset$"Total.Asset.Turnover", 
        raw_dataset$"Working.Capital.to.Total.Assets")
names <- c("Total.debt.Total.net.worth", "Debt.ratio..", "Borrowing.dependency",
        "Degree.of.Financial.Leverage..DFL.", "Operating.Profit.Per.Share..Yuan...",
        "ROA.C", "Total.Asset.Turnover", "Working.Capital.to.Total.Assets")
lapply(seq(data), function(x)
  hist(x=(data[[x]]), xlab=names(data)[x], main=paste("Histogram ", names(data)[x])))

lapply(seq(data), function(x)
  hist(x=log(data[[x]]), xlab=names(data)[x], main=paste("Histogram - log ", names(data)[x])))

lapply(data, summary)
