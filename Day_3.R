remove(list = ls())

library(tidyverse)
library(knitr)
library(broom)
library(gridExtra)
library(ggpubr)
library(ggpmisc)
library(performance)
library(qqplotr)
library(patchwork)
library(see)
library(MASS)
library(car)
library(nnet)

# Data
# Exercise 1
data(Melanoma, package = "MASS")

# Exercise 2
data(mammoexp, package = "TH.data")

# Exercise 3
data(infert, package = "datasets")

# Exercise 1
Melanoma$status <- factor(Melanoma$status, levels = c(1, 2, 3), labels = c(
  "death from melanoma", "alive", "death from other causes"))

Melanoma$ulcer <- factor(Melanoma$ulcer, levels = c(0, 1), 
                         labels = c("absent", "present"))


tab_statUlcer <- table(Melanoma$status, Melanoma$ulcer)

mosaicplot(tab_statUlcer, main = "Melanoma", xlab = "status",
           ylab = "ulceration")
tab_statUlcer_pt <- rbind(tab_statUlcer, colSums(tab_statUlcer))
tab_statUlcer_pt <- cbind(tab_statUlcer_pt, rowSums(tab_statUlcer_pt))


row.names(tab_statUlcer_pt) <- c(row.names(tab_statUlcer), "total")

colnames(tab_statUlcer_pt) <- c(colnames(tab_statUlcer), "total")

tab_statUlcer_pt

# manual OR
OR_alivedead_Malinoma <- (41 * 92) / (42 * 16)
OR_deathother_alive <- (7 * 92) / (42 * 7) 

# univariate
Melanoma$status <- relevel(Melanoma$status, ref = "alive")
Melanoma$ulcer <- relevel(Melanoma$ulcer, ref = "absent")


multinom_melanom <- multinom(status ~ ulcer, data = Melanoma)
exp(coef(multinom_melanom))
exp(confint(multinom_melanom))

# multivariate
multinom_melanom_multi <- multinom(status ~ulcer + age + sex, data =Melanoma)
summary(multinom_melanom_multi)
# the OR for melanoma stays approx the same, and the confint gets a little smaller
# the OR for other causes is smaller

# Prediction
# woman, 50, dying of melanoma, with ulcer

data_pred <- predict(multinom_melanom_multi, newdata = data.frame(
  ulcer = "present", age = 50, sex = 0
), type = "probs")

# Exercise 2
tab_SYMPT_HIST <- table(mammoexp$SYMPT, mammoexp$HIST)

mosaicplot(t(tab_SYMPT_HIST), 
           xlab = "Mother or sister with breast cancer",
           ylab = "No need for mammogram before symptoms", main = "")

tab_SYMPT_HIST <- rbind(tab_SYMPT_HIST, colSums(tab_SYMPT_HIST))
tab_SYMPT_HIST <- cbind(tab_SYMPT_HIST, rowSums(tab_SYMPT_HIST))


row.names(tab_SYMPT_HIST) <- c(
  row.names(tab_SYMPT_HIST)[-nrow(tab_SYMPT_HIST)], "total")

colnames(tab_SYMPT_HIST) <- c(
  colnames(tab_SYMPT_HIST)[-ncol(tab_SYMPT_HIST)], "total")

tab_SYMPT_HIST

# univariate regression
mod_polr_unv <- polr(SYMPT ~ HIST, data = mammoexp, method = "logistic")

# 0.0572 is the value that shows the change in log-odds to get to a higher level.
# i would say that there is little evidence to reject the null (has no influence)
# in the model we get the intercepts for each ordered "pair": so or to strongly disagree vs agree etc.

# proportionality: looks evenly spaced???

# d
multinom_mammo <- multinom(SYMPT ~ HIST, data = mammoexp)
