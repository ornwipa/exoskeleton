summary.psd <- function(signal,Fs,vis=FALSE,taskID=NULL) {
  library(stats)
  # signal is a time-domain wave
  # Fs is a sampling frequency
  Ts = 1/Fs           # sampling period
  L = length(signal)
  t = (1:L)*Ts        # time vector
  f = Fs*(1:(L/2)+1)/L  # define frequency domain
  
  Y = fft(signal)
  P2 = abs(Y/L)       # two-sided power spectrum
  P1 = P2[1:(L/2)+1]  # single-sided amplitude spectrum
  
  if (vis == TRUE) {
    plot(f, P1, xlab = "f (Hz)", ylab = "|P1(f)|", pch='.')
    title(main=paste("Power Spectral Density of EMG in",taskID))
  }
  
  psd <- cbind.data.frame(f,P1)
  fm = psd$f[which( abs(psd$P1-median(psd$P1)) == min(abs(psd$P1-median(psd$P1))) )]  
  median.power.freq = min(fm)  # use min of medain frequencies if L is even number
  return(median.power.freq)
}
