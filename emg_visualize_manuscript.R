# code for graphical results in emg manuscript
# median and IQR data were prepared outside R
emg <- read.csv(file.choose()) # emg_median_IQR.csv

for (j in 4:9) { emg[,j] <- as.double(emg[,j]) }

library(ggplot2)

ggplot(emg, aes(x=Task, y=X10thPct.median, fill=Condition)) + geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=X10thPct.median-X10thPct.IQR/2, ymax=X10thPct.median+X10thPct.IQR/2), width=.2, position=position_dodge(0.8)) + 
  scale_fill_manual(values=c("#CCCCCC","#666666")) + theme_bw() + 
  labs(title="Static Muscle Activity (10th Percentile)", x="Task", y="Muscle Activity in % RVC"); ggsave("10pct_bar.tiff")

ggplot(emg, aes(x=Task, y=X50thPct.median, fill=Condition)) + geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=X50thPct.median-X50thPct.IQR/2, ymax=X50thPct.median+X50thPct.IQR/2), width=.2, position=position_dodge(0.8)) +
  scale_fill_manual(values=c("#CCCCCC","#666666")) + theme_bw() + 
  labs(title="Median Muscle Activity (50th Percentile)", x="Task", y="Muscle Activity in % RVC"); ggsave("50pct_bar.tiff")

ggplot(emg, aes(x=Task, y=X90thPct.median, fill=Condition)) + geom_bar(stat = "identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=X90thPct.median-X90thPct.IQR/2, ymax=X90thPct.median+X90thPct.IQR/2), width=.2, position=position_dodge(0.8)) +
  scale_fill_manual(values=c("#CCCCCC","#666666")) + theme_bw() + 
  labs(title="Peak Muscle Activity (90th Percentile)", x="Task", y="Muscle Activity in % RVC"); ggsave("90pct_bar.tiff")
