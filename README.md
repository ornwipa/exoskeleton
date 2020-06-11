# Exoskeleton
R functions used for exoskeleton on farms project

Two primary steps for EMG are:

**1. Signal processing for each participant**
  * For generating exposure summary metrics, [emg.main.R](https://github.com/ornwipa/exoskeleton) processes time series data as collected in the field, uses parallel computing to process all tasks at once, its functions are:
    * [emg.filter.R](https://github.com/ornwipa/exoskeleton/blob/master/emg.filter.R) filters signal with Butterworth bandpass
    * [summary.psd.R](https://github.com/ornwipa/exoskeleton/blob/master/summary.psd.R) derrives frequency-domain parameters to help evaluate whether the signal has an error.
    * [signal.amplitude.R](https://github.com/ornwipa/exoskeleton/blob/master/signal.amplitude.R) calculates root-mean-squares of the signal
  * For visualization, [emg.presentation.R](https://github.com/ornwipa/exoskeleton/blob/master/emg.presentation.R) creates plots for individual task-participant combinations
  
**2. Statistical analysis for all participants**
  * Statistical analysis was conducted for laboratory-based simulated tasks and actual farm tasks separately
    * For simulated tasks, [emg.sim.R](https://github.com/ornwipa/exoskeleton/blob/master/emg.sim.R), Wilcoxon signed-rank tests were used
    * For farm tasks, [emg.farm.R](https://github.com/ornwipa/exoskeleton/blob/master/emg.farm.R), Shapiro-Wilk tests proves normality, and ANOVA was conducted with exoskeleton presence as fixed effect and participants and tasks are random effects.
  * For visualization, bar charts and scatter plots were made with the following codes:
    * [data_summary.R](https://github.com/ornwipa/exoskeleton/blob/master/data_summary.R) is borrowed from external source to help create graphs
    * [emg_visualize_manuscript.R](https://github.com/ornwipa/exoskeleton/blob/master/emg_visualize_manuscript.R) is applied to make graphs for publication
