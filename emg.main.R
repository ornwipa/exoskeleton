# emg data processing main code for one subject file
subj_strg <- readline(prompt = "Type subject number: ")

library(foreach)
library(doParallel)
library(matrixStats) # for summary statistics in matrices
library(tcltk) # for choosing multiple files

print("Choose all reference/calibration activities")
flist <- tk_choose.files()

# define a mxn (3x2) matrix to store the reference values
# m = number of calibration files
# n = number of muscles
emg_reference <- array(rep(NA),dim=c(3,2))

for (i in 1:length(flist)) {
  dat <- na.omit(read.table(flist[i], header=FALSE, sep="\t"))
  
  # setup parallel backend to use many processors
  cores = detectCores()
  cl <- makeCluster(cores[1]-2) # not to overload the computer
  registerDoParallel(cl)
  
  # for (j in 1:ncol(dat)) {
  tmpt <- foreach(j = 1:ncol(dat), .combine = cbind) %dopar% {
    # emg_filtered = emg.filter(dat[,j], 20, 450, 60)
    # emg_reference[i,j] = signal.amplitude(emg_filtered, 125, 1)
    signal.amplitude(emg.filter(dat[,j], 20, 450, 60), 125, 1)
  }
  emg_reference[i,] <- tmpt
  
  stopCluster(cl)
}
print("EMG calibration/reference values are:"); print(emg_reference)
ref <- colMedians(emg_reference) # required "matrixStats" package/library

print("Choose all work activities performed by this subject")
flist <- tk_choose.files()
print("The following task files were chosen:"); print(flist)

m = length(flist) # number of tasks
# n is predefined number of muscles

out <- array(dim = c(1,6))
colnames(out) = c("file","muscle","MPF","10thPct","50thPct","90thPct")

for (i in 1:m) {
  dat <- na.omit(read.table(flist[i], header=FALSE, sep="\t"))
  
  # define global variables for j, overwritten each i 
  frequency_raw <- array(data = NA, dim = c(1, ncol(dat) ) )
  emg_filtered <- array(data = NA, dim = c( nrow(dat), ncol(dat) ) )
  frequency_filtered <- array(data = NA, dim = c(1, ncol(dat) ) )
  emg_amplitude <- array(data = NA, dim = c( nrow(dat), ncol(dat) ) )
  emg_amplitude_normalized <- array(data = NA, dim = c( nrow(dat), ncol(dat) ) )
  apdf <- array(data = NA, dim = c(3, ncol(dat) ) )
  
  cores = detectCores()
  cl <- makeCluster(cores[1]-2) # not to overload the computer
  registerDoParallel(cl) # number of cluster = number of collected muscles
  
  # for (j in 1:ncol(dat)) {
  subject_task <- foreach(j = 1:ncol(dat), .combine = rbind) %dopar% {
    # check freqency component, store median power frequency, of raw data
    frequency_raw[,j] = summary.psd(as.double(dat[,j]), 1000)
    # filter EMG signal
    emg_filtered[,j] = emg.filter(dat[,j], 20, 450, 60,
    #                              vis = TRUE, taskID = paste("task",i)
                                  )
    # check freqency component, store median power frequency, of filtered data
    frequency_filtered[,j] = summary.psd(emg_filtered[,j], 1000,
    #                                     vis = TRUE, taskID = paste("task",i)
                                         )
    # calculate rms 
    emg_amplitude[,j] = signal.amplitude(emg_filtered[,j], 125, 0)
    # normalize emg amplitudes to reference value
    emg_amplitude_normalized[,j] = emg_amplitude[,j]/ref[j]
    # calculate static, median, peak of emg amplitudes
    apdf[,j] = quantile(emg_amplitude_normalized[,j], c(.1, .5, .9) )
    
    # combine: file path identification and outputs
    # run the program serially...
    # subject_task[j,] <- cbind.data.frame(flist[i],j,frequency_filtered[,j],t(apdf[,j]))
    # colnames(subject_task) = c("file","muscle","MPF","10thPct","50thPct","90thPct")
    # run the program in paralell...
    cbind.data.frame(flist[i], j, frequency_filtered[,j], t( apdf[,j]) ) # returned value
    
    # readline(prompt="Completed one task-muscle. Press [enter] to save.")
    # if ( i==1 && j==1 )
    #   write.table(subject_task, filename, append=TRUE, row.names=FALSE, col.names=TRUE)
    # else
    #   write.table(subject_task, filename, append=TRUE, row.names=FALSE, col.names=FALSE)
  }
  stopCluster(cl)
  colnames(subject_task) = c("file","muscle","MPF","10thPct","50thPct","90thPct")
  out <- rbind(out,subject_task)
}

write.csv(out, paste0("emg_analyzed_",subj_strg,".csv"), row.names = FALSE)
