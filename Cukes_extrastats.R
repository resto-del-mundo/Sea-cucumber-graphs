#PART 1: TWO-WAY ANOVA FOR TANK EFFECTS
# model <- aov(Response ~ FactorA + FactorB + FactorA:FactorB, data = data)

#1. TIRs
#ia. Two-way ANOVA for a difference between tanks (Food level)

tirsfl <- read.csv("TIR_FL.csv", header=T, sep=",") #Load data
names(tirsfl)

#ANOVA - Food level, TIRs

fl_tir_aov = aov(TIR ~ FL + Tank + Tank:FL, data = tirsfl)
summary(fl_tir_aov) #No effects at all

#ib. ANOVA - Food level, CORs

corsfl <- read.csv("COR_FL.csv", header=T, sep=",") #Load data
names(corsfl)

fl_cor_aov = aov(CORm ~ FL + Tank + Tank:FL, data = corsfl)
summary(fl_cor_aov) 

#iia. ANOVA = Temperature (all temps), cTIR
tirtemp <- read.csv("TIR_temp.csv", header=T, sep=",") #Load data
names(tirtemp)

temp_ctir_aov = aov(CTIR ~ Temperature + Tank + Tank:Temperature, data = tirtemp)
summary(temp_ctir_aov) 


#iib. ANOVA = Temperature (up to 12C), cTIR
tirtemplim <- read.csv("cTIR_temp_12.csv", header=T, sep=",") #Load data
names(tirtemplim)

temp_ctirlim_aov = aov(CTIR ~ Temperature + Tank + Tank:Temperature, data = tirtemplim)
summary(temp_ctirlim_aov) 

#iic. ANOVA = Temperature, COR

temp_cor_aov = aov(CORm ~ Temperature + Tank + Tank:Temperature, data = corstemp)
summary(temp_cor_aov) 

#PART 2: Checking ANOVA assumptions

#PART 2: Checking ANOVA assumptions

#A. CONSTANT VARIANCE
library(car) #Load library for Levene's test
# TIRs 

#Levene's test for constant variance - temp (CTIR, up to 12C)
leveneTest(CTIR ~ factor(Tank)*factor(Temperature), data = ctirtemplim) #good
#Levene's test for constant variance - temp (CTIR, all temps)
leveneTest(ctirtemp_aov)
#Levene's test for constant variance - temp (CORs)
leveneTest(CORm ~ factor(Tank)*factor(Temperature), data = corstemp) #good
#Levene's test for constant variance - FL (TIRs)
leveneTest(TIR ~ factor(Tank)*factor(FL), data = tirsfl) #good
#Levene's test for constant variance - FL (CORs)
corsfl <- read.csv("COR_FL.csv", header=T, sep=",") #Load data
names(corsfl)
leveneTest(CORm ~ factor(Tank)*factor(FL), data = corsfl) #Equal variance assumption satisfied
#P-value = 0.6454

#B. NORMALITY
#TIRs
#Checking normality - Temperature expt's, cTIRs, all temps
shapiro.test(temp_ctir_aov$residuals)
#Checking normality - Temperature expt's, CTIRs, up to 12C
shapiro.test(temp_ctirlim_aov$residuals)
#Checking normality - Temperature expt's, CORs
shapiro.test(temp_cor_aov$residuals)
shapiro.test(fl_tir_aov$residuals)
#Shapiro-Wilk test for normality - temp trials (COR)
shapiro.test(tempcors_aov$residuals) #Normality assumption met, p-value = 0.007892
#Shapiro-Wilk test for normality - FL trials (COR)
shapiro.test(fl_cor_aov$residuals) #Normality assumption met, p-value = 0.2375

# cTIR vs. Temperature, all temperatures
# 'tank' is the random factor
model <- lmer(CTIR ~ Temperature  + (1 | Tank), data = tirtemp)
summary(model)
##################

# Perform the mixed-effects ANOVA
result <- Anova(ctirtempaov, type = "III")
# Display the ANOVA table
print(result)
summary(ctirtempaov)

ctirtemp_lm <- lm(CTIR ~ Temperature, data = ctirtemp, method = "REML")

# Perform likelihood ratio test
lr_test <- anova(ctirtemp_lm, ctirtempaov)

# Display the results
print(lr_test)

#iid. Two-way ANOVA for a difference between tanks (Temperature; complete TIRs, 5 to 12C only, TANK AS A RANDOM EFFECT)

tirstemplim <- read.csv("TIR_temp_12.csv", header=T, sep=",") #Load data
names(tirstemplim)

ctirtemplim_aov = lmer(CTIR ~ Temperature + (1|Tank), data = tirstemplim)
summary(ctirtemplim_aov)


#1b CORs
#iiia. Two-way ANOVA for a difference between tanks (Food level, CORs)
flcors_aov = aov(CORm ~ FL + Tank + (Tank * FL), data = corsfl)
summary(flcors_aov) #No tank effects! :) All p-values > 0.05

#iiib. Two-way ANOVA for a difference between tanks (Temperature, CORs)
tempcors_aov = aov(CORm ~ Tank * Temperature, data = corstemp)
summary(tempcors_aov)

######################################
#########################################



#########################################################################################
########################################################################################
########################################################################################

#PART 3: Generalized linear models(GLMER)

#Example code: model <- glm(y ~ x + coeff, data=mtcars) Example code

#ia. - COR - Temperature experiments (all temps)
corstemp <- read.csv("COR_temp.csv", header=T, sep=",") #Load CORs vs. FL
names(corstemp)

glmcortemp = glm(CORm ~ Temperature + Tank + (Tank * Temperature), data = corstemp) #Linear, with tank & tank*FL as factors
summary(glmcortemp)

#ib. - COR - Temperature experiments (up to 12C)

corstemplim <- read.csv("COR_temp_12.csv", header=T, sep=",") #Load CORs (up to 12C) vs. FL
names(corstemplim)

glmcortemplim = glm(CORm ~ Temperature + Tank + (Tank * Temperature), data = corstemplim) #Linear, with tank & tank*FL as factors
summary(glmcortemplim)

#iia. cTIRs - Temperature (all temperatures)

ctirtemp = read.csv("cTIRtemp.csv", header=T, sep=",") #Load cTIRs vs. temp
names(ctirtemp)

glmctirtemp = glm(CTIR ~ Tank, data = ctirtemp) #Linear, with tank & tank*FL as factors
summary(glmctirtemp)

#iib. cTIRs - Temperature (up to 12C only)
ctirtemplim = read.csv("cTIR_temp_12.csv", header=T, sep=",") #Load cTIRs vs. temp (up to 12C)
names(ctirtemplim)

#Population feeding (avg. % fully open) - Temperature (GLM) XXXXXXXXXXXx

#####################
#########################
##############################

#CHECKING FOR TANK EFFECTS
#Comparing different lmer models


#LMMs - lmer

#LMMs - Temperature
#CTIR vs. Temp - up to 12C only, Tank as a random effect
modellim = lmer(CTIR ~ Temperature + (1 | Tank), data = tirtemplim)
summary(modellim)
# Obtain the AIC
aic.ran <- AIC(modellim)

# Print the AIC value
print(aic.ran)

######################
########################

###################################

#CTIR vs. Temp - up to 12C only, no random effects
model2 <- lm(CTIR ~ Temperature, data = tirtemplim)
model2
# Obtain the AIC
aic.noranlim <- AIC(model2)
# Print the AIC value
print(aic.noranlim)

#########################
#CORs vs. Temperature - Tank as a random effect
modelcortemp <- lmer(CORm ~ Temperature + (1 | Tank), data = corstemp)
summary(modelcortemp)

#FOOD LEVEL - TIRs vs. FL (random effects; linear)
modelranfllin <- lmer(TIR ~ FL + (1 | Tank), data = tirsfl)
summary(modelranfllin)
AIC(modelranfllin)  # AIC: 99.77681

#FOOD LEVEL - TIRs vs. FL (random effects; quadratic)
modelranflq <- lmer(TIR ~ FL^2 + (1 | Tank), data = tirsfl)
summary(modelranflq)
AIC(modelranflq)  # AIC: 99.77681
################################
################################

# ii - GLM vs lmer w a random effect - Likelihood ratio tests


#Temperature vs. cTIR (all temps), GLM + likelihood ratio test
full_model <- lmer(CTIR ~ Temperature + (1 | Tank), data = tirtemp)
null_model <- lmer(CTIR ~ Temperature, data = tirtemp)
lr_test <- lrtest(null_model, full_model) # Perform likelihood ratio test
print(lr_test)

#Temperature vs. cTIR - up to 12C, GLM + likelihood ratio test
library(lmtest)
names(ctirtemplim)

full_modellim <- lmer(CTIR ~ Temperature + (1 | Tank), data = ctirtemplim)
null_modellim <- lmer(CTIR ~ Temperature, data = ctirtemplim)

# Perform likelihood ratio test
lr_test <- lrtest(null_model, full_model)

# Display the results
print(lr_test)

#Try the anova approach again
# Perform likelihood ratio test using anova
lr_test_anova <- anova(null_model, full_model, test = "Chisq")

# Display the results
print(lr_test_anova)

##########################################
##########################################



ranova_resultlim <- ranova(modellim)
print(ranova_resultlim)

#Check the normality of the residuals
residuals = residuals(modellim)
shapiro.test(residuals) #Data are normal!

#EXTRA#
#Extract model information (to inform graph)
summary_model <- summary(model)
fixed_effects <- fixef(model)
r_squared <- summary_model$r.squared #Returns: NULL
r_squared ###########xxxxx
#########################
#CTIR vs. Temp - all temps, Tank as a random effect, Tank: Temperature as a random effect, lmer
model0 <- lmer(CTIR ~ Temperature + (1 | Tank) + (1 | (Tank:Temperature)), data = tirtemp)
summary(model0)
AIC(model0) # AIC: 65.95033
##################
################
#CTIR vs. Temp - all temps, no random effect
model1 <- lm(CTIR ~ Temperature, data = ctirtemp)
summary(model1)
aic.noran <- AIC(model1) # Obtain the AIC
print(aic.noran) # AIC: 52.20098 (same if ran as a GLM)
##############
#CTIR vs. Temp - all temps, with Tank as a fixed effect 
model2 <- lm(CTIR ~ Temperature + Tank, data = tirtemp)
summary(model2)
aic.noranf <- AIC(model2) # Obtain the AIC
print(aic.noranf) # AIC: 52.00722 (same if ran as a GLM)
##############
#CTIR vs. Temp - all temps, with Tank as a fixed effect and Tank:Temperature interaction
model3 <- lm(CTIR ~ Temperature + Tank + (Tank * Temperature), data = tirtemp)
summary(model3) #Highest adj. R2 (0.4444)
aic.noranfi <- AIC(model3) # Obtain the AIC
print(aic.noranfi) # AIC: 39.51591 (same if ran as a GLM)
##############
##############
##############


#The stacked bar graph:
pft = ggplot(data = pftemp, aes(x = Day, y = Percentage, fill = factor(Status, levels = c("Closed", "Partially", "Fully")))) +  
  geom_col() +
  scale_fill_manual(
    name = "Crown extension status",
    values = c("#D55E00", "#0072B2", "#009E73"),
    labels = c("Closed", "Partially open", "Fully open")
  ) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  scale_y_continuous(
    name = "Average % of population",
    sec.axis = sec_axis(trans = ~./5, name = "Temperature (°C)")
  ) +  # Add a second axis and specify its features
  geom_point(aes(y = Temperature*5), show.legend = FALSE) +
  # geom_line(aes(y = Temperature*5), col = "black", linetype = "dashed", size = 1) +
  scale_color_identity(
    name = '',
    breaks = c('black'),
    labels = c("Temperature (C)")
  ) +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 12), #Set font size
        axis.text = element_text(family = "Times New Roman", color = "black", size = 12),
        legend.title.align = 0.5, #Center legend title
        plot.background = element_rect(color = "gray", fill = NA, size = 0.75),  # Grey border around the plot
        panel.background = element_rect(fill = "white")) +  # White background inside the grey border
  labs(x = "Day", y = "% of population")  # Set axis labels

###########################
#1d. TIRs vs. Phytoplankton concentration (up to 30 000 cells/ml only)

#TIRs - food level (up to 30 000 cells/ml)
#TIRs - Food level 
tirsfllim = read.csv("TIR_FL_30000.csv")
names(tirsfllim)

tirflim = ggplot(data = tirsfllim, aes(x = FL, y = TIR)) +
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), se = F, linetype = "dashed", color = "black") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE), use_label("R2")) +
  geom_jitter(aes(color = as.factor(Tank)), show.legend = F) + #jitter to unstack data points
  geom_smooth(aes(group = Tank, color = as.factor(Tank)), method = "lm", linewidth = 0.75, linetype = "dashed", se = FALSE) + # Individual tank trendlines
  theme(axis.line = element_line(colour = "black"),
        axis.title.x = element_text(color = "black", size = 13),
        axis.title.y =element_text(color = "black", size = 13),
        axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(colour="black", size = 12)) +
  labs(x = "Phytoplankton concentration (cells/ml)", y = "TIRs (insertions/min)") +
  scale_color_manual(values =c("blue","darkorange"), labels=c("Tank 1", "Tank 2"))
tirflim

#Save TIR vs. FL (up to 30 000 cells/ml)
ggsave("C:/Users/map_w/Desktop/K/R_cukes/FL experiments_graphs/Main/TIR_FL_up to 30 000cells/ml_by tank.jpeg",
       width = 10,
       height = 6,
       units = "in",
       device = 'jpeg',
       dpi = 500)

#1b. Complete TIRs vs. temp (up to 12C only)
ctirtemplim <- read.csv("cTIR_temp_12.csv", header=T, sep=",")
names(ctirtemplim)

ctirtemp.lim = ggplot(data = ctirtemplim, aes(x = Temperature, y = CTIR))+
  stat_poly_line(formula = y ~ poly(x, 1, raw = TRUE), se = F, linetype = "dashed", color = "black") +
  stat_poly_eq(formula = y ~ poly(x, 1, raw = TRUE),use_label("R2")) +
  geom_point(aes(color = as.factor(Tank)), show.legend = F) + #Set colour scheme but remove legend
  geom_smooth(aes(group = Tank, color = as.factor(Tank)), method = "lm", linewidth = 0.75, linetype = "dashed", se = FALSE) + # Individual tank trendlines
  theme(axis.line = element_line(colour = "black"),
        axis.title.x = element_text(color = "black", size = 13),
        axis.title.y =element_text(color = "black", size = 13),
        axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(colour="black", size = 12),
        legend.title = element_blank()) +
  labs(title = "", x = "Temperature (°C)", y = "cTIR (insertions/min)") +
  scale_color_manual(values =c("blue","darkorange"), labels=c("Tank 1", "Tank 2"))
ctirtemp.lim

#Save cTIR vs. Temp (up to 12C)
ggsave("C:/Users/map_w/Desktop/K/R_cukes/Temp experiments_graphs/Main/cTIR_up to 12C_by tank.jpeg",
       width = 10,
       height = 6,
       units = "in",
       device = 'jpeg',
       dpi = 500)

##########################
##########################

#Other graphs

# TIRs vs. CORs, Temperature (complete TIRs only)
tirtemp <- read.csv("TIR_temp.csv", header=T, sep=",") #Load data
names(tirtemp)

#Scatterplot
comptircors = ggplot(data = tirtemp, mapping = aes(x = COR.m, y = CTIR)) + 
  stat_poly_line(se = F, linetype = "dashed", color = "black") +
  stat_poly_eq(use_label("R2")) +
  geom_point(aes(color = Temperature)) + #Colour code by temp
  theme(axis.line = element_line(colour = "black", size = 1),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y =element_text(color = "black",size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(colour="black", size = 12),
        legend.title = element_blank()) +
  scale_color_viridis(option = "D") + #Cooler to warmer gradient color palette
  theme_classic() +
  labs(x = "COR (openings/min)", y = "cTIR (insertions/min)")
comptircors

# TIRs vs. CORs, FL
tirfl <- read.csv("TIR_FL.csv", header=T, sep=",") #Load data
names(tirfl)

#Scatterplot
tircorsfl = ggplot(data = tirfl, mapping = aes(x = CORm, y = TIR)) + 
  stat_poly_line(se = F, linetype = "dashed", color = "black") +
  stat_poly_eq(use_label("R2")) +
  geom_point(aes(color = FL)) + #Colour code by temp
  theme(axis.line = element_line(colour = "black", size = 1),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y =element_text(color = "black",size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.text.x = element_text(colour="black", size = 12),
        legend.title = element_blank()) +
  scale_color_viridis(option = "A") + #Cooler to warmer gradient color palette
  theme_classic() +
  labs(x = "COR (openings/min)", y = "TIR (insertions/min)")
tircorsfl




############################################
#Subset into Tank 1, Tank 2 & the average ("Tank 3")
envi.t1 <- subset(envi, Tank == 1)
envi.t2 <- subset(envi, Tank == 2)
envi.av <- subset(envi, Tank == 3)

# Set graphical parameters for text
par(family = "Times New Roman", cex = 1.2, cex.axis = 1.2)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(envi.av$Day, envi.av$Temp, pch=21, axes=FALSE, ylim=c(4,17), xlab="", ylab="", 
     type="b", main="") #No title above figure
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Temperature (°C)",side=2.5,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(envi.av$Day, envi.av$Tank.C, pch=19,  xlab="", ylab="", ylim=c(1000,30000), 
     axes=FALSE, type="b")
## a little farther out (line=4) to make room for labels
mtext("Phytoplankton concentration (cells/ml)",side=4,line=4) 
axis(4, ylim=c(0,7000), col="black",las=2)

## Draw the time axis
axis(1, 1:39) #axis(1) adds x-axis
mtext("Day of experiment",side=1,col="black",line=2.5) 

## Add Legend
legend("topleft",legend=c("Temperature","Phytoplankton concentration"),
       pch=c(21,19),col=("black"),
       inset = 0.05, # Distance from the margin as a fraction of the plot region
       bty = "n") # bty = n emoves the legend box))

########################################
