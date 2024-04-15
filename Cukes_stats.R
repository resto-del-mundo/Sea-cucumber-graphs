rm(list=ls())

library(ggplot2)
library(lme4)
library(car)
library(lmerTest)
library(nlme)
library(MuMIn) #For getting conditional (fixed + random effects) and marginal (fixed effects only) R2 values from lmer()

############################################################################

#Load data
#1. Temperature experiment

#TIR
tirtemp <- read.csv("TIR_temp.csv", header=T, sep=",") #Load data
names(tirtemp)

#TIR up to 12C
tirtemplim <- read.csv("cTIR_temp_12.csv", header=T, sep=",") #Load data
names(tirtemplim)

#COR
corstemp <- read.csv("COR_temp.csv", header=T, sep=",") #Load data
names(corstemp)

#COR up to 12C
corstemplim <- read.csv("COR_temp_12.csv", header=T, sep=",") #Load data
names(corstemplim)

#Population feeding - Temperature
popftr <- read.csv("popf_temp_third.csv", header=T, sep=",") #Load data
names(popftr)

#2. Food level experiment

#TIR
tirsfl <- read.csv("TIR_FL.csv", header=T, sep=",") #Load data
names(tirsfl)

#COR
corsfl <- read.csv("COR_FL.csv", header=T, sep=",") #Load data
names(corsfl)

#Population feeding
popfflr <- read.csv("popf_FL_third.csv", header=T, sep=",") #Load data
names(popfflr)


#Pooled morphometrics
morph <- read.csv("Morph_pooled.csv", header=T, sep=",") #Load data
names(morph)

#TIR & COR at 8C and 10,000 cells/ml
tirexpt = read.csv("TIRexpt.csv", header=T, sep=",") #Load data
corexpt = read.csv("CORexpt.csv", header=T, sep=",")

####################################
####################################

#T-tests (both experiments)

#T-tests: morphometrics
t.test(DW ~ Expt, data = morph)
t.test(IW ~ Expt, data = morph)
t.test(CL ~ Expt, data = morph)
t.test(WW ~ Expt, data = morph)
t.test(V ~ Expt, data = morph)
t.test(V ~ Sex_num, data = morph)
t.test(CORm ~ Sex_num, data = tirsfl)

#T-tests: difference in COR across experiments
t.test(COR ~ Expt, data = corexpt, alternative = "greater")

############################################################################
############################################################################

#1. Population feeding

#LMM - Fully open
modelpftf <- lmer(Fully ~ Temperature + (1 | Tank), data = popftr) #Tank as a random effect
#LMM - Closed
modelpftc <- lmer(Closed ~ Temperature + (1 | Tank), data = popftr) #Tank as a random effect

########################
#1b. Population feeding - Food level

#LMM - Fully open
modelpfff <- lmer(Fully ~ FL + (1 | Tank), data = popfflr) #Tank as a random effect
#LMM - Closed
modelpffc <- lmer(Closed ~ FL + (1 | Tank), data = popfflr) #Tank as a random effect

############################
############################
#Temperature experiment

#Individual feeding - temperature

      #1. Linear model, no random effect
      modelfull <- lm(CTIR ~ Temperature, 
             data = tirtemp)

      summary(modelfull)
      
    #Template function to estimate cTIR at every temperature
    temptir = function(T) { 0.6 + (0.07*T)}
    temptir(7.6)

      #2. Linear Mixed-Effect model, Tank as a random effect
      model1 <- lme(CTIR ~ Temperature, 
             random = ~ 1 | Tank, 
             data = corstemp)
      
      summary(model1)

#Model fit
AIC(model)

#Equal variance
grouped_residuals <- residuals(model)
group_variable <- factor(tirtemp$Tank)  # Residuals associated with the random effect
car::leveneTest(grouped_residuals, group_variable) #Levene's test

#Normality
shapiro.test(residuals(model)) #Shapiro-Wilk test

residuals <- resid(model) #Check normality of residuals
qqnorm(residuals) 
qqline(residuals) 

#Tank effects (only for mixed-effect models)
ranova(model1)
###########################
#cTIRs vs. Temp - up to 12C (trying different models)
modellim <- lm(CTIR ~ Temperature, data = tirtemplim) #Tank as a random effect

summary(modellim)


##################################################
##################################################

#Metabolism (COR) - Temperature

#COR vs. TEMP
modelfull = lm(CORm ~ T, data = corstemp)

summary(modelfull)

anova(modelfull) #Extract model parameters

######################
# COR vs. TEMP (up to 12C)
modellim = lm(CORm ~ Temperature, data = corstemplim)

summary(modellim)

anova(modellim) #Extract model parameters

# 95% confidence interval
confint(modellim, level = 0.95)

#Template function to estimate COR at every temperature
tempcor = function(T) { 1.83 + (0.19*T)}
tempcor(6)

##################################################
##################################################

#Individual feeding - Food level experiment 

# Michaelis-Menten model for cTIR vs. [Phytoplankton]
MMmodel <- function(FL, Vm, K) { Vm * FL / (K + FL) }  #Define the MM model

#1. RANDOM EFFECTS, nlme
fit <- nlme(TIR ~ my_model(FL, Vm, K), 
            fixed = Vm + K ~ 1, 
            random = Vm ~ 1 | Tank, 
            data = tirsfl, 
            start = c(Vm = max(tirsfl$TIR), K = median(tirsfl$FL))) 
summary(fit)

#2. NO RANDOM EFFECTS, nls
fit <- nls(TIR ~ MMmodel(FL, maxTIR, FoodK), #Fit the model
           data = tirsfl,
           start = c(maxTIR = max(tirsfl$TIR), FoodK = median(tirsfl$FL)))

#Template function to estimate cTIR at every food level
fltir = function(P) { (1.29*P) /(P +2499)}
fltir(2999.049)

# Obtain the summary of the nls model
summary_fit <- summary(fit)

# Calculate R-squared
RSS <- sum(summary_fit$residuals^2)
TSS <- sum((tirsfl$TIR - mean(tirsfl$TIR))^2)
R_squared <- 1 - (RSS / TSS)

# Print R-squared
print(R_squared)
###################################

#Ingestion - food level

#Type 1 - linear
ing1 <- lm(Ingestion ~ FL, data = tirsfl)

summary(ing1)
AIC(ing1)

###################################
#Type 2
MMmodel <- function(FL, Vm, K) { Vm * FL / (K + FL) } 

ing2 =  nls(Ingestion ~ MMmodel(FL, Vm, K), #Fit the model
            data = tirsfl,
            start = c(Vm = max(tirsfl$Ingestion), K = median(tirsfl$FL)))

summary(ing2)
AIC(ing2)

#Obtain pseudo-R2
summary_fit = summary(ing3)
RSS <- sum(summary_fit$residuals^2)
TSS <- sum((tirsfl$Ingestion - mean(tirsfl$Ingestion))^2)
R_squared <- 1 - (RSS / TSS)
R_squared #Pseudo-R2 = 0.62
##########################

#Type 3
MMmodel3 <- function(FL, Vm, K, R) { Vm * FL^R / (K + FL^R) } 

ing3 =  nls(Ingestion ~ MMmodel3(FL, Vm, K, R), #Fit the model
             data = tirsfl,
             start = c(Vm = max(tirsfl$Ingestion), K = median(tirsfl$FL), R = 1))

summary(ing3)

#Obtain AIC
AIC(ing3)

#Obtain pseudo-R2
RSS <- sum(summary_fit$residuals^2)
TSS <- sum((tirsfl$Ingestion - mean(tirsfl$Ingestion))^2)
R_squared <- 1 - (RSS / TSS)
R_squared #Pseudo-R2 = 0.62

###################################
###################################

#COR - Food level experiment

#A. lme with random effects

model <- lme(CORm ~ FL, 
             random = ~ 1 | Tank, 
             data = corsfl)
summary(model)

#Tank effects
ranova(model)

#B. lm without random effects
model1 <- lm(CORm ~ FL, 
             data = corsfl)
summary(model1)

#############################
##########################
######################
#Q: Does cTIR increase linearly with FL up to 30,000 cells/ml?
tirfllim <- read.csv("TIR_FL_30000.csv", header=T, sep=",") #Load data
names(tirfllim)

summary(lm(TIR ~ FL, data = tirfllim))

#Visualize TIR vs FL
ggplot(data = tirsfl, aes(x = FL, y = TIR)) + 
  geom_point(show.legend = FALSE)
#Up to 30000 cells/ml: R2 = 0.26, Adj. R2 = 0.24; AIC = 33.92 
#Up to 20000 cells/ml: R2 = 0.38, Adj. R2 = 0.35; AIC = 24.87

#Q: Does COR increase asymptotically from 5 to 16C?
names(corstemp)
ggplot(data = corstemp, aes(x = Temperature, y = CORm)) + 
  geom_point(show.legend = FALSE)

#Arrhenius model
# Define the Arrhenius equation as a function
arrhenius <- function(T, A, Ea) {
  R <- 8.314 # Gas constant in J/(mol*K)
  return(A * exp(-Ea / (R * T)))
}

temperature <- corstemp$T

oxygen_consumption <- corstemp$CORm

# Fit the Arrhenius model to the dataset
fit <- nls(oxygen_consumption ~ arrhenius(temperature, A, Ea), #T, A, Ea
           start = list(A = 1, Ea = 1))

# Print the summary of the model fit
summary(fit)  #Super low p-values; AIC = 473.79
AIC(fit)

# Plot the data and the fitted model
plot(temperature, oxygen_consumption, xlab = "Temperature (K)", ylab = "Rate Constant", 
     main = "Arrhenius Model Fit")
lines(temperature, predict(fit), col = "red")

#