#required packages: 

##tidyverse = data wrangling and visualization, 
##sjPlot = to visualizing mixed-effects models, 
##lme4 ="golden standard" for mixed-effects modelling in R (no p-values),
##lmerTest = p-values for MEMs based on the Satterthwaite approximation,
##emmeans = post-hoc analysis
##knitr = beautifying tables
##sjstats = ICC - intraclass-correlation coefficient
##MASS
##r2glmm


pkgs <- c("tidyverse", "sjPlot", "lme4", 
          "lmerTest", "emmeans", "knitr",
          "sjstats","MASS","r2glmm","effects","car")

# install.packages if needed 
#install.packages(pkgs)

#Load all the required packages
lapply(pkgs, library, character.only = TRUE)

#read in data
df <- read.csv("titanic.csv")

#explore data
View(df)
head(df)
str(df)

#change variables to factor
df_new <- df %>%
  mutate(pclass = factor(pclass),
         survived = if_else(survived == 1, "no", "yes"),
         survived = factor(survived, levels = c("no", "yes")),
         Gender= if_else(Gender == 0, "male", "female"),
         Gender= factor(Gender, levels = c("male", "female")))

head(df_new)

#checking missing values
df_new %>%
  summarise_each(list(~sum(is.na(.)))) %>%
  gather() # 263 NA's in age


list(df_new$age)

#remove missing values
df_clean<- df_new %>% 
  drop_na(age)

df_clean %>%
  summarise_each(list(~sum(is.na(.)))) %>%
  gather() # 

#sample size after removing data

df_clean %>% 
  summarise(n = n()) #1046

#Number of survived by gender, class

df_clean %>%
  group_by(Gender) %>%
  count(survived)

df_clean %>%
  group_by(pclass) %>%
  count(survived)

#logit - multiple regression
logit.m1 <- glm(survived ~ pclass + 
                  Gender, data = df_clean, 
                family = binomial(link = "logit"))

#stepwise selection
logit.step <- stepAIC(logit.m1, direction = "both")

logit.step$anova


# multicollinearity
car::vif(logit.m1)

#model2 - interaction
logit.m2 <- glm(survived ~ pclass + Gender + pclass: 
                  Gender, data = df_clean, 
                family = binomial(link = "logit"))
summary(logit.m2)


#AIC
anova(logit.m1, logit.m2)

# multicollinearity
car::vif(logit.m2)

#random intercept model

logit.mx1 <- glmer(survived ~ pclass + Gender + pclass:Gender
                   + (1 | age),
                   family = binomial(link = "logit"), 
                   data = df_clean )
summary(logit.mx1)


#does not violate multicolinearity
car::vif(logit.mx1)

#interaction is significant and should be included in the model
#R(m)2, the proportion of variance explained by the fixed predictors. 
r2.mx = r2beta(logit.mx1, method = 'nsj', partial = TRUE)
r2.mx

#model validation
logit.mxV <- glmer(survived ~ 1 
                   + (1 | age),
                   family = binomial(link = "logit"), 
                   data = df_clean )

performance::icc(logit.mxV)## suggests that random intercept of age does not explain that much variation in model
MuMIn::r.squaredGLMM(logit.mx1)

anova(logit.mx1,logit.mxV, test ="Chisq")

#pseudo r-squared for GLM
MuMIn::r.squaredLR(logit.m2)

#Controlling for age as a random factor does not improve model fit and is not needed in the model
#Therefore the best model is a logistic multiple regression (logit.m2)

#multiple comparisons for simple logistic multiple regression
tab_model(logit.m2, show.aic = T)
emmeans(logit.m2, pairwise ~ pclass | Gender, adjust = "bonferroni")$contrasts 
emmeans(logit.m2, pairwise ~ pclass:Gender)

#visualize
#Visualize the interactions between social class and gender

plot(allEffects(logit.m2))

##visualize the residuals to see if there are any differences in the variability of residuals as the value for each predictor variable increases.
residualPlots(logit.m2, plots = T)

##Save the Interaction plot
# 1. Open jpeg file
jpeg("InteractiveEffectsPlot.jpg")
# 2. Create the plot
plot(allEffects(logit.m2))
# 3. Close the file
dev.off()

##visualize the residuals to see if there are any differences in the variability of residuals as the value for each predictor variable increases.
residualPlots(logit.m2, plots = T)


##Save the residuals plot
# 1. Open jpeg file
jpeg("ResidualsPlot.jpg")
# 2. Create the plot
residualPlots(logit.m2, plots = T)
# 3. Close the file
dev.off()


#model table for mixed model to see ICC etc.
tab_model(logit.mx1, show.aic = T)


#How well does the model classify - model validation bonus
Pred <- predict(logit.m2, type = "response")
Pred <- if_else(Pred > 0.5, 1, 0)
ConfusionMatrix <- table(Pred, pull(df_clean, survived)) #`pull` results in a vector
#correct classification rate
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)#model correctly classifies 78%
ConfusionMatrix 