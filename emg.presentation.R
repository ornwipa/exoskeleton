# This script is to create graphical presentation
dat <- na.omit(read.table(file.choose(), header=FALSE, sep="\t"))
signal <- dat[,1]

nyqf = 500
f.cut.low = 20
f.cut.high = 300
bp <- butter(n=2, W=c(f.cut.low/nyqf,f.cut.high/nyqf), type="pass")
signal.filt <- filtfilt(bp,signal)
f.env = 60
for (k in 1:floor(f.cut.high/f.env)) {
  bs <- butter(n=2, W=c((f.env*k-0.5)/nyqf,(f.env*k+0.5)/nyqf), type="stop")
  signal.filt <- filtfilt(bs,signal.filt)
}

win = 125
filtrms <- Arma( b=rep(1/win,win), a=c(1,rep(0,win-1)) )
root.mean.square <- sqrt(filter(filtrms, signal.filt^2, sides=1, methods="convolution"))

plot(signal, col="green3", type="l", ylab="Amplitude (uV)", xlab="Time (ms)")
lines(signal.filt, col="blue", lty=3)
lines(root.mean.square[62:(length(root.mean.square)+124)], col="black")
abline(h=mean(root.mean.square), col="magenta")

plot(ecdf(root.mean.square))
abline(h=c(.1, .5, .9))
apdf = quantile(root.mean.square, c(.1, .5, .9) )
