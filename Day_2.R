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

# Load the csv file
dat<-read.csv2("data_caerphilly_full.csv")
str(dat)

# make factors out of character variables
dat<- dat %>% 
  mutate(mi = factor(mi, levels=c(0, 1), labels=c("No", "Yes")),
         socclass = factor(socclass, levels=c("I", "II", "IIINM", "IIIM", "IV", "V")), 
         diabetes=factor(diabetes, levels=c("No/uncertain", "Yes")), 
         smoking=factor(smoking, levels=c("Never smoked", " Ex>5 years", "Ex 1-4 years" ,"<15 per day", ">15 per day")),
         hbpsyst = factor(hbpsyst, levels=c(0, 1), labels=c("No", "Yes")),
         hbpdias = factor(hbpdias, levels=c(0, 1), labels=c("No", "Yes")),
         bmicat = factor(bmicat, levels=c("Underweight", "Normal", "Overweight", "Obese")),
         cursmoke = factor(cursmoke, levels=c("No", "Yes")),
         fibrin= as.numeric(fibrin),
         totchol=as.numeric(totchol),
         hdlchol=as.numeric(hdlchol),
         bpdias=as.numeric(bpdias),
         bmi=as.numeric(bmi),
         bpsyst=as.numeric(bpsyst),
  )

# Variable seleciton
vars<-c("socclass", "diabetes", "cursmoke", "smoking", "fibrin", "totchol", "hdlchol", "bpsyst", "bpdias", "bmi" )

# univariate models

varname <- c()
pval <- c()
vars<-c("socclass", "diabetes",  "smoking", "cursmoke","fibrin", "totchol", "hdlchol", "bpsyst", "bpdias", "bmi" )

for (var in vars) {
  formula <- paste("mi ~", var )
  model<-glm(formula, data=dat, family=binomial(link="logit"))
  pval <- c(pval, anova(model, test="Chisq")[["Pr(>Chi)"]][2])
}
tab <- data.frame(vars,pval, pval<=0.20)
names(tab) <- c("Variable", "P-value LR-test", "Include (p<=0.2)")
kable(tab, digits=6)

# for the next step, i would include all the variables. for the final model, we dont know yet.

# step 2: full model

mod_f<-glm(mi ~ socclass + diabetes + cursmoke + smoking + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit"))
summary(mod_f)

# no convergance for smoking > 15?
# --> no if you look at multicolinaerity, you see that it is perfectly collinear with cursmoke. drope one variable

mod_f_alt<-glm(mi ~ socclass + diabetes  + smoking + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit"))
summary(mod_f_alt)

# Which variables to exclude: for categorical there are no clear interpretations, if for each category we haven't the same result. 
# If for one category we have p < 0.1 and for another we don't, then i think it might be worthwile to try excluding them...
# so i would test socclass, and smoking. 

mod_r1<-glm(mi ~ diabetes  + smoking + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit")) # without socclass
summary(mod_r1)

anova(mod_r1, mod_f_alt, test="Chisq") 
# p value is high, we cannot reject the null --> simpler model is as well as complicated

mod_r2<-glm(mi ~ socclass + diabetes  + fibrin + totchol + hdlchol + bpsyst + bpdias + bmi, data=dat, family=binomial(link="logit")) # without smoking
summary(mod_r2)

anova(mod_r2, mod_f_alt, test="Chisq") 
# there is evidence to reject the null --> we should take more complicated model.
# i would further test to take out bmi and bpsyst, as wald z test suggests no relevance.

# new reduced model
mod_r<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
anova(mod_r,  mod_f_alt, test="Chisq") 
# again the p value suggests that we have very little evidence to reject the null --> we keep simple model
glance(mod_f_alt)
glance(mod_r)
# the bic is smaller in the reduced model, as is the aic --> suggests better with reduced

# step 5
# define a factor variable representing quintils of fibrin
quint <- quantile(dat$fibrin, seq(0,1,0.2))
dat$fibrin_q <- cut(dat$fibrin, quint, include.lowest = TRUE)
# a numeric variable representing medians within quintiles
qmed <- tapply(dat$fibrin, dat$fibrin_q, median)
dat$fibrin_qm <- qmed[dat$fibrin_q]
# run regressions
temp1<-glm(mi ~ diabetes + smoking + fibrin_qm+ totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
temp2<-glm(mi ~ diabetes + smoking + fibrin_q + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
# Create the data to plot
new <- dat[1,] |> dplyr::select(diabetes, smoking, totchol,hdlchol, bpdias) # Takes out 1 covariate pattern which we will keep constant
new <- cbind(new, fibrin_q=levels(dat$fibrin_q), fibrin_qm=qmed) # add the quintile variables
new$fit1<-predict(temp1, newdata=new) # add the predicted logit 
new$fit2<-predict(temp2, newdata=new) # add the predicted logit 

# Plots logit scale
ggplot(data=new, aes(x=fibrin_qm)) +
  geom_vline(xintercept = quint, linetype="dotted",  color = "blue") +
  geom_line(aes(y=fit1), color="red") +
  geom_line(aes(y=fit2), col="blue") +
  xlab("fibrin") + ylab("logit") +
  geom_text(data = NULL, x = 5, y = -1.55, label = "Linear on quintile medians", color="red", size = 5, hjust="left" ) +
  geom_text(data = NULL, x = 5, y = -1.45, label = "Unrestricted on quintiles", color="blue", size = 5, hjust="left") +
  theme_minimal() +
  theme(text = element_text(size = 20))
# the curve for unrestricted regression on quintiles is quite close to red line --> i would assume linearity.

# second way with loess
# 1
temp<-glm(mi ~ diabetes + smoking + fibrin + totchol + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
# 2
dat$fit <- temp$fitted.values
# 3, 4
dat$mi_n <- as.numeric(dat$mi=="Yes")
ggplot(data=dat, aes(x=fibrin, y=mi_n))+
  geom_point(alpha = 1/10) + 
  geom_smooth(method="loess", se=TRUE) + 
  #  geom_line(data=new, aes(y=fit, x=fibrin), color="red") +
  geom_smooth(aes(y=fit), method="loess",  color="red", se=FALSE) +
  xlab("fibrin") + ylab("probability") +
  geom_text(data = NULL, x = 1, y = .7, label = "LOESS of predictions based on linear logits", color="red", size = 5, hjust="left" ) +
  geom_text(data = NULL, x = 1, y = .8, label = "LOESS of outcome", color="blue",  size = 5, hjust="left") +
  theme_minimal() +
  theme(text = element_text(size = 20)) 
# also here the red line fits well to the loess --> as we have probabilities on y axis, we see the CDF (?): so it's normal that in to the right is gets a little lower.

# step 6: interactions
mod_r_int<-glm(mi ~ diabetes + smoking * totchol + fibrin + hdlchol +  bpdias , data=dat, family=binomial(link="logit"))
summary(mod_r_int)
# we already see high z wald tests

anova(mod_r, mod_r_int, test="Chisq") # -> we cannot reject the null --> keep simple model

# interpretation of full model
# For each unit change in fibrin the probability is higher by 44% to get outcome. 
# would be easier if we take the exp --> so we get ORs
# Smoking: keeping other variables constant, if you were a smoker 5 years ago, you have 1.5 higher ods for MI.
# as the CIs include 1 --> we cannot prove this, and the evidence is too weak to reject the null.
# contratry to hdl --> here we have evidence to reject the null.

new <- data.frame(diabetes="Yes", smoking="<15 per day", fibrin=3, totchol=5,hdlchol=0.1,bpdias=8)

prediction<-predict(mod_r, newdata=new, type = "response")
prediction
