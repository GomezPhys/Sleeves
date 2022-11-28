### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad



##### CK_and_Cortisol

library(readxl)
Sleeves_1 <- read_excel("~/Ergonomics Lab (PERROS 69)/Sleeves CK_Cortisol/Sleeves 1.xlsx",
                        sheet = "Cortisol")
View(Sleeves_1)

## Convert from character to factor data
Sleeves_1$Condition <- as.factor(Sleeves_1$Condition)
Sleeves_1$Time <- as.factor(Sleeves_1$Time)
Sleeves_1$Workload <- as.factor(Sleeves_1$Workload)

##Order workload
class(Sleeves_1$Workload)


## Order Time
Sleeves_1$Time <- ordered(Sleeves_1$Time, levels = c("PRE", "POST"))

## Order Condition
Sleeves_1$Condition <- ordered(Sleeves_1$Condition, levels = c("S", 'NS'))

##Concentration Normality
Sleeves_1 %>% group_by(Condition) %>%
  shapiro_test(Concentration)

##### Linear Mixed models Concentration
lmModel = lmer(Concentration ~ Condition + Time + (1|Subject_ID),
data=Sleeves_1, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Sleeves_1 %>%
  pairwise_t_test(Concentration ~ Condition, paired = FALSE,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Sleeves_1 %>% cohens_d(Concentration ~ Condition,
                       paired = FALSE, hedges.correction = FALSE)%>%

  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Condition")
# Boxplot of Concentration
Concentration_plot <- ggboxplot(Sleeves_1, x = "Condition", y = "Concentration",
                                color = "Workload",
                                fill = "Time",
                                palette = get_palette("Set2",
                                                  10),
                                ylab = "Concentration") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("Concentration_plot.png")


