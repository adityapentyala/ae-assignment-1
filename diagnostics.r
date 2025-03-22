library(car)
library(caret)
library(dplyr)
library(erer)
library(gvlma)
library(readxl)
library(tidyr)
library(writexl)
library(pscl, lib.loc = "C:/Program Files/R/R-4.4.2/library")

vif_altman_log <- vif(probit_model_altman_log)
print(vif_altman_log)

mcfadden_altman_log <- pR2(probit_model_altman_log)
cat("McFadden's R² for probit_model_altman_log:", round(mcfadden_altman_log['McFadden'], 4), "\n")

reset_test_altman_log <- resettest(probit_model_altman_log, power=2, type="fitted")
print(reset_test_altman_log)

# Wald's Test
waldtest(probit_model_altman_log)

# AUC-ROC curve test
predicted_probs_log <- predict(probit_model_altman_log, type="response")
roc_altman_log <- roc(raw_dataset$Bankrupt. , predicted_probs_log)
auc_value <- auc(roc_altman_log)
print(auc_value)

# Link Test
raw_dataset$predicted <- predict(probit_model_altman_log), type='link')
raw_dataset$predicted_sq <- raw_dataset$predicted^2
link_test <- glm(Bankrupt. ~predicted + predicted_sq, family=binomial(link="probit"), data=raw_dataset)