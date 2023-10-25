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

# Load data
perulung <- read_csv("perulung_ems.csv", show_col_types = FALSE)

# Exercise 1
head(perulung, 10)

# Exercise 2

perulung <- perulung  %>%  
  mutate(id = as.integer(id),
         sex = factor(sex, levels = c(0,1), labels=c("f","m")),
         respsymptoms = factor(respsymptoms, levels=c(0,1), labels=c("no","yes")),
         asthma_hist = factor(asthma_hist, levels=c("never", "previous asthma", "current asthma"), 
                              labels=c("never", "previous asthma", "current asthma")))

# Exercise 3

plot_fev1 <- ggplot(perulung, aes(x = fev1)) + geom_histogram()
plot_height <- ggplot(perulung, aes(x = height)) + geom_histogram()

plot_scatter <- ggplot(perulung, aes(x = height, y = fev1)) + geom_point()

# Exercise 4
pearson_fev_height <- cor(perulung$fev1, perulung$height)

# Exercise 5
x = perulung$height
y = perulung$fev1
beta1 <- (cor(y,x) * sd(y))/sd(x)
beta0 <- mean(y) - beta1*mean(x)

# Exercise 6
lm_fev_height <- lm(fev1~height, data = perulung)

# Exercise 7
predict(lm_fev_height, newdata = data.frame(height = 113.8))

# Exercise 8
summary_lm <- tidy(lm_fev_height)

# Excercise 9
glance_lm <- glance(lm_fev_height)
# 40% is explained.

# Exercise 10
t_test <- summary_lm$estimate / summary_lm$std.error

p <- 2*pt(-abs(t_test), glance_lm$df.residual)
p 

# Exercise 11
x_norm <- (x - mean(x)) / sd(x)
y_norm <- (y - mean(y)) / sd(y)

lm_stand <- lm(y_norm ~ x_norm)

# Exercise 12
plot_scatter + stat_poly_line(se = F) + stat_poly_eq(use_label(c("eq"))) +
  stat_poly_eq(use_label(c("adj.R2")), label.y = 0.9) + theme_bw()

# Exercise 13
# Rename the columns to have more descriptive names
birthwt <- MASS::birthwt %>%
  rename(birthweight_below_2500 = low,
         mother_age = age,
         mother_weight = lwt,
         ethnicity = race,
         mother_smokes = smoke,
         previous_prem_labor = ptl,
         hypertension = ht,
         uterine_irr = ui,
         physician_visits = ftv,
         birthweight = bwt)

# Recode the factors
birthwt <- birthwt %>% 
  mutate(birthweight_below_2500 = factor(birthweight_below_2500, levels = c(0,1), labels=c("no","yes")),
         ethnicity = factor(ethnicity, levels = c(1,2,3), labels=c("White","Black", "Other")),
         mother_smokes = factor(mother_smokes, levels = c(0,1), labels=c("no","yes")),
         hypertension = factor(hypertension, levels = c(0,1), labels=c("no","yes")),
         uterine_irr = factor(uterine_irr, levels = c(0,1), labels=c("no","yes")))

birthwt %>% 
  head(5) %>% 
  kable()

lm_univ <- lm(birthweight ~ mother_smokes, data = birthwt)
# for smokers birthwt is 283 g lower (of babies). r squared is very low, only small amount of variance can be explained.

lm_2 <- lm(birthweight ~ mother_smokes + mother_weight, data = birthwt)
summary(lm_2)
# r squared is always higher when you add another predictor (you decrease df). if mother is heavier so is baby.
# smokes does still deacrease brithweight of baby.

lm_3 <- lm(birthweight ~ mother_smokes + mother_weight + 
             hypertension + mother_age, data = birthwt)
summary(lm_3)
# smokes no is reference
# r saured is 0.11 --> so not that much of variance is explained. 
# if mother smokes that reduces birth weight in the mean of 263 g
# if hypertensive during pregancy that reduces bwt with a mean of 579g.


anova(lm_3, lm_2)
# P value is 0.02. we have quite strong evidence to reject the null hypothesis that the reduced and full model are the same.

par(mfrow = c(2, 2))
plot(lm_3)

check_model <- check_model(lm_3, panel = FALSE, check = "linearity")
plot(check_model) 
# if we look at the CI of the green line (probably non-parametric estimation of mean):
# we see that this estimated mean is still close to zero and looks reasonably flat.

# exercise 14
normality <- check_normality(lm_3)
plot(normality, type = "qq")
# the dots follow the equality line nicely in the middle. at the edges, where we have 
# fewer results we have more discrepancies. we would be happy with that result still.

# exercise 15
heterosc <- check_heteroscedasticity(lm_3)
plot(heterosc)
# At the edges we have deviations, but in the middle it is fine. the data points follow
# normal distribution around mean. 

# exercise 16
outliers.mod <- check_outliers(lm_3, threshold = list("cook" =1))
plot(outliers.mod)
# we have data points with high leverage, but these are within the OK bounderies of cook distance
# we have evidence that these high leverage points will not influence the regression.

# exercise 17
vif(lm_3)
# we have quite small values --> reason to believe that we have no collinearity
# also from the clinical point of view we would not suspect collinearity and we have
# not observed any weird changes in betas during the model building. 