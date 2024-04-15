rm(list=ls())

# Load libraries
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(reshape2)
library("viridis")
library("scales")
library(patchwork)

theme_set(theme_classic())

########################################################
#Load data
#1. TEMPERATURE

#Environment
envi_t <- read.csv("Environment_temp.csv", header=T, sep=",")
names(envi_t)

#TIR
tirtemp <- read.csv("TIR_temp.csv", header=T, sep=",") #Load data
names(tirtemp)

#TIR up to 12C
tirtemplim <- read.csv("cTIR_temp_12.csv", header=T, sep=",") #Load data
names(tirtemplim)

#Novel behaviours (OP and iTIR)
feedt <- read.csv("Beh_temp_noctir.csv", header=T, sep=",")
feedt <- na.omit(feedt) #Get rid of NAs
names(feedt)

#COR
corstemp <- read.csv("COR_temp.csv", header=T, sep=",") #Load data
names(corstemp)

#Population feeding
pftemp = read.csv("popf_temp_third.csv", header=T, sep=",") #Third day only
names(pftemp)

#########################

#2. FOOD LEVEL

#Environment
envi.fl <- read.csv("Environment_FL.csv", header=T, sep=",") #Load data
names(envi.fl)

#TIR - no model TIR
tirsfl <- read.csv("TIR_FL.csv", header=T, sep=",") #Load data
names(tirsfl)

#TIR - with model TIR
tirsflwm = read.csv("TIR_FL_wm.csv")
names(tirsflwm)

#COR
corsfl <- read.csv("COR_FL.csv", header=T, sep=",") #Load data
names(corsfl)

#Population feeding
pffl = read.csv("popf_FL_third.csv")
names(pffl)

###############################

#3. - APPENDIX

#TIR - time accuracy A (video lengths)
timeacc <- read.csv("TIRs_time accuracy.csv", header=T, sep=",")
timeacc = na.omit(timeacc) #Remove NAs
names(timeacc)

#TIR - time accuracy B (4min sections)
fourmin= read.csv("TIRs_4min.csv", header=T, sep=",")
names(fourmin)

#Fully open x TIR (all temperature)
fullytir <- read.csv("FullyxTIR_full.csv", header=T, sep=",")
names(fullytir)

#Fully open x TIR (5 to 13C only)
fullytirlim <- read.csv("FullyxTIR_lim.csv", header=T, sep=",")
names(fullytirlim)
#############################################
#############################################

#1 - ENVIRONMENT

#Environment - temperature
envitemp = ggplot(envi_t, aes(x = Day)) +
  geom_point(aes(y = Temperature, shape = "Temperature"), size = 3, color = "black", fill = "white") +
  geom_line(aes(y = Temperature), linetype = "dashed") +
  
  geom_line(aes(y = FL/3100), linetype = "dashed") +
  geom_point(aes(y = FL/3100, shape = "Food"), size = 3, color = "black", fill = "white") +
  
  scale_shape_manual(values = c(16, 1), labels = c("Phytoplankton", "Temperature"), name = "") +
  scale_color_manual(values = c("black", "black", "black")) +
  #annotate("text", x = 1.3, y = 17.1, label = "A", size = 6.5, fontface = "bold") + #Add letter A to top left corner
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(trans = ~.*3100, name = bquote(bold("Phytoplankton (cells ml"^{-1} * ")"))),
    limits = c(0, 18),  # Set the limits of the primary y-axis
    expand = c(0, 0.05)) +  # Expand the limits to ensure the end value is visible
  scale_x_continuous(breaks = seq(0, max(envi_t$Day), by = 5)) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5, face = "bold"),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
        legend.position = c(0.08, 0.97), legend.justification = c(0, 1),
        legend.key = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 13.5),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add panel border
        axis.title.y.right = element_text(angle = 90, hjust = 0.5)) +   # Rotate the secondary y-axis label
        theme(legend.background = element_blank()) +  # Remove legend background
        labs(x = "Day")
envitemp

############################################

#Environment - food level
envifl = ggplot(envi.fl, aes(x = Day)) +

  geom_point(aes(y = Temp, shape = "Temperature"), size = 3, color = "black", fill = "white") +
  geom_line(aes(y = Temp), linetype = "dashed")+
  
  geom_point(aes(y = FL/3100, shape = "FL"), size = 3, color = "black", fill = "white") +
  geom_line(aes(y = FL/3100), linetype = "dashed") +
  scale_shape_manual(values = c(16, 1), labels = c("Phytoplankton", "Temperature"), name = "") +
  scale_color_manual(values = c("black", "white")) +
  #annotate("text", x = 1.3, y = 17.1, label = "B", size = 6.5, fontface = "bold") + #Add letter B to top left corner
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(trans = ~.*3100, name = bquote(bold("Phytoplankton (cells ml"^{-1} * ")"))),
    limits = c(0, 18),  # Set the limits of the primary y-axis
    expand = c(0, 0.05)) +  # Expand the limits to ensure the end value is visible
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  theme_classic() +
    theme(axis.line = element_line(colour = "black"),
          axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5, face = "bold"),
          axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
          legend.position = c(0.08, 0.97), 
          legend.justification = c(0, 1),
          legend.key = element_blank(),
          legend.text = element_text(family = "Times New Roman", size = 13.5),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Add panel border
          axis.title.y.right = element_text(angle = 90, hjust = 0.5, face = "bold")) +   # Rotate the secondary y-axis label
          theme(legend.background = element_blank()) + # Remove legend background
          labs(x = "Day")
envifl

##########################################################

#Arrange the two environment figures side-by-side

envir = grid.arrange(envitemp, envifl, nrow = 1)
envir

#Save the bipanel graph
ggsave("C:/Users/memli/Desktop/Sea cucumbers/R_data analysis/Temp experiments_graphs/Main/Overall Environment_noletters.jpeg",
       plot = envir,
       width = 275,
       height = 100,
       units = "mm",
       device = 'jpeg',
       dpi = 500)

##########################################################
#########################################################

#2 - CLOACAL RATES (COR)

#COR - TEMPERATURE
cort = ggplot(data = corstemp, aes(x = T, y = CORm)) +  #Full dataset:corstemp, 5 to 12C only: corstemplim
  geom_point(show.legend = FALSE) +
  geom_segment(aes(x=5.25,xend=12,y=2.829,yend=4.1029),           #Fit the linear model
               linetype = "solid", linewidth = 0.6, color = "black") + #Using geom_segment instead of geom_abline to only draw up to 12C
  annotate("text", x = 7, y = 5.9, 
           label = "COR = 2.18 ± 0.15 + 0.14 ± 0.01 x T",            #Describe the linear model
           fontface = "plain", family = "Times New Roman", size = 5, hjust = 0.5) +
  annotate('text', x = 7, y = 5.6,
           label=paste("~italic(p < 0.001)~'  '~italic(r)^2==", 0.34), 
           parse=TRUE, 
           family = "Times New Roman",
           hjust=0.5, size=5) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 14, face = "bold"),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 14),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(x = "Temperature (°C)", y = bquote(bold("COR (openings min"^{-1} * ")"))) +
  scale_x_continuous(limits = c(4, 16), breaks = seq(0, 20, by = 2)) +  # Customize x-axis major tick marks
  scale_y_continuous(breaks = seq(0, 10, by = 0.75))  # Customize y-axis major tick marks

cort
#########################################################
#COR - Food level
corphyto = ggplot(data = corsfl, aes(x = FL, y = CORm)) +
  geom_point() +
  geom_abline(intercept = 2.864, slope = 0.0000005852, linetype = "dashed", color = "black", size = 0.6) + #Fit the linear model 
  theme(axis.line = element_line(colour = "black", size = 0.5),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(x = bquote(bold("Phytoplankton concentration (cells ml"^{-1} * ")")), y = bquote(bold("COR (openings min"^{-1} * ")"))) +
  scale_x_continuous(limits = c(0, 52000), 
                     breaks = c(2000, 10000, 20000, 30000, 40000, 50000),
                     labels = c("2,000", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  lims(y = c(2, 4.5))  # Set y-axis limits
corphyto

############################################
############################################

#3. INDIVIDUAL FEEDING

#Novel behaviours - Temperature
indbehpos <- ggplot(data = feedt, aes(x = Temperature, y = Rate, shape = Behaviour)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(15, 3), labels = c("iTIR", "OP")) +
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(limits = c(0, 2.5),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  scale_x_continuous(limits = c(4, 16), 
                     breaks = c(4, 8, 12, 16)) +
  #annotate("text", x = 4, y = 2.45, label = "B", size = 6, fontface = "bold") + #Add letter B to top left corner
  theme_classic() +
  theme(
    axis.title = element_text(family = "Times New Roman", color = "black", size = 22),
    axis.text = element_text(family = "Times New Roman", color = "black", size = 22),
    axis.title.x = element_text(family = "Times New Roman", color = "black", size = 22, face = "bold"),
    axis.line = element_line(linewidth = 1),
    legend.title.align = 0.5, #Center legend title
    legend.title = element_text(family = "Times New Roman", size = 22),
    legend.position = c(0.1, 0.96),
    legend.justification = c(0, 1),
    legend.text = element_text(family = "Times New Roman", size = 22),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # Add panel border
    legend.background = element_rect(color = "black", linewidth = 1)) +
  labs(x = "Temperature (°C)", y = bquote(bold("Individual rate (min"^{-1} * ")")))
indbehpos

###############################################

#Complete tentacle insertions (cTIR) - Temperature
complete <- ggplot(data = tirtemp, aes(x = Temperature, y = CTIR)) +
  geom_point(show.legend = FALSE, size = 2) +
  scale_y_continuous(limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  scale_x_continuous(limits = c(4, 16), 
                     breaks = c(4, 8, 12, 16)) +
  annotate("text", x = 9, y = 2.3, 
           label = "cTIR = 0.60 ± 0.11 + 0.07 ± 0.01 x T",  #Describe the linear model
           fontface = "plain", family = "Times New Roman", size = 8, hjust = 0.5) +
  annotate('text', x = 9, y = 2.1,
           label = paste("~italic(p < 0.001)~'  '~italic(r)^2==", 0.34), 
           parse = TRUE, 
           family = "Times New Roman",
           hjust = 0.5, size = 8) +
  #annotate("text", x = 4, y = 2.45, label = "A", size = 6, fontface = "bold") + #Add letter A to top left corner
  geom_segment(aes(x = 5.25, y = 0.99, xend = 15.8, yend = 1.785),     #Fit the linear model
               linetype = "solid", size = 1, color = "black") +  
  theme_classic()+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 22, face = "bold"),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 22),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.background = element_rect(color = "black", linewidth = 1),  # Add legend box
        legend.position = c(0.05, 0.96)) +
        labs(x = "Temperature (°C)", y = bquote(bold("cTIR (insertions min"^{-1} * ")")))
complete

########################################################

#Display cTIR & novel behaviours side-by-side
feeding =grid.arrange(complete, indbehpos, nrow = 1)

#######################################################
#Complete tentacle insertions (cTIR) - Food level
tirfl = ggplot(data = tirsflwm, aes(x = FL, y = TIR)) +
  geom_point(show.legend = F) +
  geom_line(aes(x=Model_FL, y = Model_TIR), size = 0.75) +   # Fit the Michaelis-Menten model
  annotate("text", x = 26000, y = 2.32, 
           label = "cTIR = 1.29 ± 0.09 x P/(P + 2499 ± 1489)",  #Describe the Michaelis-Menten model
           fontface = "plain", family = "Times New Roman", size = 5, hjust = 0.5) +
  annotate("text", x = 26000, y = 2.17, 
           label = paste("~italic(p < 0.001)~' '~italic(r)^2==", 0.15), 
           parse = TRUE, 
           family = "Times New Roman",
           hjust = 0.5, size = 5) +
  annotate("text", x = 4, y = 2.45, label = "A", size = 6, fontface = "bold") + #Add letter A to top left corner
  theme_classic() +
  theme(axis.line = element_line(colour = "black", size = 0.5),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5),
        axis.text.x = element_text(size = 13.5),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Add panel border
  labs(x = bquote(bold("Phytoplankton concentration (cells ml"^{-1} * ")")), y = bquote(bold("cTIR (insertions min"^{-1} * ")"))) +
         scale_x_continuous(breaks = c(2000, 10000, 20000, 30000, 40000, 50000),
                            labels = c("2,000", "10,000", "20,000", "30,000", "40,000", "50,000"))  #Manually set food level breaks
tirfl

#############################################
#Ingestion - Food level
names(tirsflwm)
names(tirsfl)

ing = ggplot(data = tirsfl, aes(x = Model_FL)) +
  geom_point(aes(x = FL, y = Ingestion)) +
  geom_line(aes(y = Ing_one, linetype = "Type 1"), size = 0.75, color = "black") +     # Fit the linear (Type 1) model
  geom_line(aes(y = Ing_two, linetype = "Type 2"), size = 0.75, color = "black") +      # Fit the Type 2 model
  geom_line(aes(y = Ing_three, linetype = "Type 3"), size = 0.75, color = "black") +  # Fit the Type 3 model
  scale_linetype_manual(name = "Functional response", 
                        values = c("solid", "dashed", "dotted"),
                        labels = c("Type I", "Type II", "Type III")) +
  theme_classic() +
  annotate("text", x = 400, y = 102000, label = "B", size = 6, fontface = "bold") + #Add letter B to top left corner
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5, face = "bold"),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
        legend.position = c(0.08, 0.97),
        legend.justification = c(0, 1),
        legend.text = element_text(family = "Times New Roman", size = 13.5),
        legend.title = element_text(family = "Times New Roman", size = 13.5),
        legend.title.align = 0.5,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(x = bquote(bold("Phytoplankton concentration (cells ml"^{-1} * ")")),
       y = bquote(bold("Proxy for ingestion rate (~cells min"^{-1} * ")"))) +
  scale_y_continuous(breaks = c(0, 20000, 40000, 60000, 80000, 100000, 120000),
                     labels = c("0", "20,000", "40,000", "60,000", "80,000", "100,000", "120,000")) +  #Manually set food level breaks
  scale_x_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000, 60000),
                     labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000", "60,000")) #Manually set ingestion breaks
ing

#############################################
#Arrange individual feeding and ingestion into a bi-panel graph
food =grid.arrange(tirfl, ing, nrow = 1)
#############################################

#4. POPULATION FEEDING

#Population feeding - Temperature
pft = ggplot(data = pftemp, aes(x = factor(Day), y = Percentage, fill = factor(Status, levels = c("Closed", "Partially", "Fully")))) +  
  geom_col() +
  scale_fill_manual(
    name = "Crown extension",
    values = c("#D55E00", "#0072B2", "#7FFF00"),
    labels = c("Closed", "Partially open", "Fully open")) +
  scale_x_discrete(labels = c("5.2","6.0", "6.7", "7.8", "8.9", "10.1", "11.0", "12.0", "13.1", "14.0", "15.2", "15.8")) +
  scale_y_continuous(limits = c(0, 102), expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black", size = 1),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 22), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust vjust for vertical alignment
        axis.text = element_text(family = "Times New Roman", color = "black", size = 14),
        legend.title.align = 0.5,
        legend.text = element_text(size = 18, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        panel.background = element_rect(fill = "white")) +
  labs(x = bquote(bold("Temperature (°C)")), y = bquote(bold("Average % of population")))  # Set axis labels
pft

#Save population feeding - temperature
ggsave("C:/Users/memli/Desktop/Sea cucumbers/R_data analysis/Temp experiments_graphs/Main/Population feeding - Temperature.jpeg",
       plot = pft,
       width = 220,
       height = 208 * 2/3,
       units = "mm",
       device = 'jpeg',
       dpi = 500)

##########################################

#Population feeding - Food level 
pff = ggplot(data = pffl, aes(x = factor(Day), y = Percentage, fill = factor(Status, levels = c("Closed", "Partially", "Fully")))) +  
  geom_col() +
  scale_fill_manual(
    name = "Crown extension",
    values = c("#D55E00", "#0072B2", "#808080"),
    labels = c("Closed", "Partially open", "Fully open")) +
      scale_x_discrete(labels = c("2,100", "11,470", "21,360", "29,920", "44,980", "51,280")) +
      scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  theme(axis.line = element_line(colour = "black", size = 1),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 22), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust vjust for vertical alignment
        axis.text = element_text(family = "Times New Roman", color = "black", size = 14),
        legend.title.align = 0.5,
        legend.text = element_text(size = 18, family = "Times New Roman"),
        legend.title = element_text(size = 20, family = "Times New Roman"),
        panel.background = element_rect(fill = "white")) +
      #theme(axis.line = element_line(colour = "black"),
            #axis.title = element_text(family = "Times New Roman", color = "black", size = 13.5, face = "bold"), 
            #axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 13.5),  # Adjust vjust for vertical alignment
            #axis.text = element_text(family = "Times New Roman", color = "black", size = 13.5),
            #legend.title.align = 0.5,
            #legend.text = element_text(size = 13),
            #legend.title = element_text(size = 13),
            #panel.background = element_rect(fill = "white")) +
  labs(
    x = bquote(bold("Phytoplankton concentration (cells ml"^{-1} * ")")),
    y = bquote(bold("Average % of population")))  # Set axis labels
pff

########################################################
########################################################

#APPENDIX FIGURES

#TIRs vs. video length - accuracy
tirstime = ggplot(data = timeacc, aes(x = Video.length, y = TIRs, shape = as.factor(Video.number))) + 
  geom_point(size = 2, color = "black", show.legend = T) +
  geom_line(aes(group = as.factor(Video.number)), linetype = "dashed", color = "black", size = 0.75) +
  scale_shape_manual(values = c(19, 21, 15, 2)) +
  scale_y_continuous(
    limits = c(1, 2.5),
    breaks = c(1, 1.5, 2, 2.5)) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", size = 0.5) +  # Add vertical line
  annotate("text", x = 1, y = 2.5, label = "A", size = 6.5, fontface = "bold") + #Add letter A to top left corner
   theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.title.y = element_text(family = "Times New Roman", color = "black", size = 14, face = "bold"), 
        axis.title.x = element_text(family = "Times New Roman", color = "black", size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 14),
        axis.text = element_text(family = "Times New Roman", color = "black", size = 14),
        legend.title.align = 0.5, #Center legend title
        legend.title = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.background = element_blank(),
        legend.box = "horizontal",  # Set legend box orientation to horizontal
        legend.box.just = "center", 
        legend.position = c(0.93, 0.39), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +  # Add panel border
  labs(x = "Video length (min)", y = bquote(bold("cTIR (insertions min"^{-1} * ")"))) +
  labs(shape = "Video")   # Set legend title

#####################################################

#Spread in TIR over different 4-min sections
fourmintirs = ggplot(data = fourmin, mapping = aes(x = factor(Videonum), y = TIR)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(aes(group = Videonum)) +
  labs(x = "Video #", y = bquote(bold("cTIR (insertions min"^{-1} * ")"))) +
  geom_point() + #Add individual data points
  scale_y_continuous(
    limits = c(1, 2.5),
    breaks = c(1, 1.5, 2, 2.5)) +
  annotate("text", x = 0.6, y = 2.5, label = "B", size = 6.5, fontface = "bold") + #Add letter B to top left corner
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(family = "Times New Roman", color = "black", size = 14, face = "bold"), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 14),  # Adjust vjust for vertical alignment
        axis.text = element_text(family = "Times New Roman", color = "black", size = 14),
        legend.title.align = 0.5,
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white"))

###############################

#Arrange the two appendix figures side by side
tirspread =grid.arrange(tirstime, fourmintirs, nrow = 1)

#########################################################################################
#########################################################################################
