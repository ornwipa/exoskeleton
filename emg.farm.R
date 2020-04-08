#### Re-analysis using Generalized Estimating Equations or Generalized Linear Models

setwd("~/VirtualBox VMs/shared-folder")
emg.farm <- na.omit(read.csv(file.choose(), header=TRUE)) # emg_farm.csv

shapiro.test(log(emg.farm$X10thPct[emg.farm$Condition=="No Exo"])) # p-value = 0.1006
shapiro.test(log(emg.farm$X50thPct[emg.farm$Condition=="No Exo"])) # p-value = 0.04439
shapiro.test(log(emg.farm$X90thPct[emg.farm$Condition=="No Exo"])) # p-value = 0.1708

shapiro.test(log(emg.farm$X10thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.08981
shapiro.test(log(emg.farm$X50thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.1725
shapiro.test(log(emg.farm$X90thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.1398

#shapiro.test(sqrt(emg.farm$X10thPct[emg.farm$Condition=="No Exo"])) # p-value = 0.1002
#shapiro.test(sqrt(emg.farm$X50thPct[emg.farm$Condition=="No Exo"])) # p-value = 0.06892
#shapiro.test(sqrt(emg.farm$X90thPct[emg.farm$Condition=="No Exo"])) # p-value = 9.397e-05

#shapiro.test(sqrt(emg.farm$X10thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.0292
#shapiro.test(sqrt(emg.farm$X50thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.7642
#shapiro.test(sqrt(emg.farm$X90thPct[emg.farm$Condition=="With Exo"])) # p-value = 0.002898

# install.packages("gee")
library(gee)

# install.packages("lme4")
library(lme4)
# install.packages("lmerTest")
library(lmerTest)
# https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/

anova(lmer(log(X10thPct) ~ Condition + (1|Participant) + (1|Task), data = emg.farm)) # 0.03288 *
anova(lmer(log(X50thPct) ~ Condition + (1|Participant) + (1|Task), data = emg.farm)) # 0.0248 *
anova(lmer(log(X90thPct) ~ Condition + (1|Participant) + (1|Task), data = emg.farm)) # 0.1854

