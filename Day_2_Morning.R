library(epiDisplay)

CC_noccardia <- read_csv2("nocardia_updated.csv")

#specifiy the following variables as factor
CC_noccardia<- CC_noccardia %>%
  mutate(dneo = factor(dneo, levels=c(0, 1), labels=c("No", "Yes")),
         dclox = factor(dclox, levels=c(0, 1), labels=c("No", "Yes")),
         casecont = factor(casecont, levels=c(0, 1), labels=c("Control", "Case")))


CC_noccardia %>% 
  head() %>% 
  kable()

# Exploratory data analysis
#2*2 tables
tabpct(CC_noccardia$casecont, CC_noccardia$dclox)
# it looks like that neomycin is harmful and clox is protective 
# total percentages do not make sense, because we do not have a population. 
# also depening on how the decision was made to treat the herds, maybe there is bias?

# conditional distribution plot: i.e. when 80% of the heard get treatment then 60% are cases. so the more are treated the more likely you are to be a case. 
# Can you deduce the reverse causation from this plot? my thinking is that the probability of receiving treatment should be always 50%?
# No <ou ccant deduce it form this --> you have to know the design. 

# Exercise 3
mod1 <- glm(casecont ~ dneo,family = binomial(link="logit"), data = CC_noccardia)
summary(mod1)

# equation: beta0 + beta1*neomycin
# if you had neomycicn this will increase the probability to have mastitis by 2.34 on the logit scale.
# take the exp of the estimate and then you get the odds. 
# if you take neomycin your odds are 11x higher to have mastitis

# if a heard is treated with neomycin its probability to have mastitis is 60%
# not relevant, because we have no representative population.
# in a cohort study we could then say that if you get neomycin your probability to get mastitis is 60%.

# exercise 4
# dpct: if you get any treatment then the probability is increased by 0.01
# the increase in the odds to get mastitits

# exercise 5
# we see confounding when we add clox --> the neo estimate gets a little lower
# if we add the treatment i would expect multicolinearity, because they say the same thing?

# exercise 6
# neomyc: your odds increase by 24 to get mastitis if you have neomyc. 
# clox: your odds get lower by 0.156 when you get clox
# interaction: if you get both clox and neomyc then your odds decrease by 0.07 additionally to the other effects.

# not considering any flaws in the design, only clox should be taken, as we have this huge increase in mastitis whith neomycin

# i would suspect multicolinearity. as the estimates change loads. but also we 
# decrease the sample size, which increases the variance. also the effect of clox alone
# in the interaction model is now irrelevant --> so it might just have changed sign by chance...
