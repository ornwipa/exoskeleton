emg.filter <- function(signal,f.cut.low,f.cut.high,f.env,vis=FALSE,taskID=NULL) {
  library(signal)
  # signal is a raw emg signal
  nyqf = 500 # Nyquist frequency = (sampling frequency)/2 = 500 Hz
  
  # filter: Butterworth 2nd order bandpass 20-450 Hz
  # f.cut.low = 20, and f.cut.high = 450
  bp <- butter(n=2, W=c(f.cut.low/nyqf,f.cut.high/nyqf), type="pass")
  signal.filt <- filtfilt(bp,signal)
  
  # filter: notch (band stop) at 60 Hz for environmental noise and harmonic
  for (k in 1:floor(f.cut.high/f.env)) {
    bs <- butter(n=2, W=c((f.env*k-0.5)/nyqf,(f.env*k+0.5)/nyqf), type="stop")
    signal.filt <- filtfilt(bs,signal.filt)
  }
  
  # visualize signal in frequency and time domain
  if (vis == TRUE) {
    # library(astsa) # Applied Statistical Time Series Analysis
    # mvspec(signal,log="no")
    # mvspec(signal.filt,log="no")
    
    plot(signal, col="green3", type="l", ylab="Amplitude (uV)", xlab="Time (ms)")
    lines(signal.filt, col="blue", lty=3)
    title(main=paste("Raw and Filtered EMG signals in",taskID))
  }

  return(signal.filt)
}
