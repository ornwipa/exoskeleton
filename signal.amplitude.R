signal.amplitude <- function(signal,win,ref) {
  library(signal)
  # signal is a time-domain wave; filtered emg signal
  # win is the length of overlapping windows, 125 ms for emg
  # ref is a binary variable: 
  # ref=1 indicates reference/calibration, ref=0 indicates other work activities
  
  filtrms <- Arma( b=rep(1/win,win), a=c(1,rep(0,win-1)) )
  root.mean.square <- sqrt(filter(filtrms, signal^2, sides=1, methods="convolution"))
  
  if (ref==0) {
    # if ((win %% 2) == 0) {win = win+1}
    # rectified.median <- runmed(abs(signal), win) /(2/pi)
    # return two-column realization of rms and rectified median
    # return(cbind.data.frame(root.mean.square,rectified.median))
    return(root.mean.square) # return only rms signal, exclude rectified median
  }
  
  if (ref==1) { 
    ref.rms <- mean(as.double(root.mean.square)) 
    return(ref.rms)
  }
}
