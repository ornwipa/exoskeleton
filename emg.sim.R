# new code for simulated tasks

#### Read and organize dataset ####

emg <- read.csv(file.choose()) # emg_simulated_task.csv for re-analysis lab participants
# subset for pilot partcipant only
# pilot <- c("pilot01","pilot02","pilot03","pilot04","pilot05","pilot06")
# emg <- subset.data.frame(emg, emg$Participant %in% pilot)

# subset for each simulated task
emg.sim1 <- subset.data.frame(emg, emg$Task=="1 Symmetric Lifting")
emg.sim2 <- subset.data.frame(emg, emg$Task=="2 Asymmetric Lifting")
emg.sim3 <- subset.data.frame(emg, emg$Task=="3 Static Bending")

#### Run required packages ####

# install.packages("pkgconfig")
library(ggplot2)

#### visualize data for publication ####

emg.participant <- read.csv(file.choose())  # emg_simulated_task_participant_regroup.csv
emg.sim1 <- subset.data.frame(emg.participant, emg.participant$Task=="1 Symmetric Lifting")
emg.sim2 <- subset.data.frame(emg.participant, emg.participant$Task=="2 Asymmetric Lifting")
emg.sim3 <- subset.data.frame(emg.participant, emg.participant$Task=="3 Static Bending")

ggplot(emg.sim1, aes(x=NoExo10thPct, y=UseExo10thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() +
  labs(title="Symmetric Lifting, 10th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim1_10pct_scat_group.tiff")

ggplot(emg.sim1, aes(x=NoExo50thPct, y=UseExo50thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() +
  labs(title="Symmetric Lifting, 50th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim1_50pct_scat_group.tiff")

ggplot(emg.sim1, aes(x=NoExo90thPct, y=UseExo90thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() +
  labs(title="Symmetric Lifting, 90th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim1_90pct_scat_group.tiff")

ggplot(emg.sim2, aes(x=NoExo10thPct, y=UseExo10thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) + 
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Asymmetric Lifting, 10th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim2_10pct_scat_group.tiff")

ggplot(emg.sim2, aes(x=NoExo50thPct, y=UseExo50thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Asymmetric Lifting, 50th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim2_50pct_scat_group.tiff")

ggplot(emg.sim2, aes(x=NoExo90thPct, y=UseExo90thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Asymmetric Lifting, 90th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim2_90pct_scat_group.tiff")

ggplot(emg.sim3, aes(x=NoExo10thPct, y=UseExo10thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) + 
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Static Bending, 10th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim3_10pct_scat_group.tiff")

ggplot(emg.sim3, aes(x=NoExo50thPct, y=UseExo50thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Static Bending, 50th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim3_50pct_scat_group.tiff")

ggplot(emg.sim3, aes(x=NoExo90thPct, y=UseExo90thPct, shape=Group)) + geom_point(size=2) + scale_shape_manual(values=c(5,3)) +
  geom_abline(intercept=0, slope=1) + scale_color_grey() + theme_classic() + 
  labs(title="Static Bending, 90th%tile", x="No Exoskeleton", y="Use Exoskeleton"); ggsave("sim3_90pct_scat_group.tiff")

#### Re-test stat ####

wilcox.test(emg.sim1$NoExo10thPct, emg.sim1$UseExo10thPct, paired=TRUE) 
wilcox.test(emg.sim1$NoExo50thPct, emg.sim1$UseExo50thPct, paired=TRUE) 
wilcox.test(emg.sim1$NoExo90thPct, emg.sim1$UseExo90thPct, paired=TRUE) 

wilcox.test(emg.sim2$NoExo10thPct, emg.sim2$UseExo10thPct, paired=TRUE) 
wilcox.test(emg.sim2$NoExo50thPct, emg.sim2$UseExo50thPct, paired=TRUE) 
wilcox.test(emg.sim2$NoExo90thPct, emg.sim2$UseExo90thPct, paired=TRUE) 

wilcox.test(emg.sim3$NoExo10thPct, emg.sim3$UseExo10thPct, paired=TRUE) 
wilcox.test(emg.sim3$NoExo50thPct, emg.sim3$UseExo50thPct, paired=TRUE) 
wilcox.test(emg.sim3$NoExo90thPct, emg.sim3$UseExo90thPct, paired=TRUE) 
