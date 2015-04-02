#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

probs <- c(0.1,0.3,0.5,0.6,0.7,0.8,0.9)

## Returns matrix that lists timestep count before Gridlock or Freeflow limit for 1000 trials
## for each density in the traffic model. Note: p can take ina vector of probabilities.
SimData <- function(r,c){
  TimeStepMat <- matrix(ncol=length(probs), nrow = 1000)
  for (i in 1:length(probs)){
    TimeStepMat[,i] <- replicate(1000, bml.sim(r,c,probs[i])[[2]])}
  return(TimeStepMat)
  }

## Returns a vector that lists the average timestpes it took for a certain car density in the
## traffic model for those triasl that hit gridlock before 1000 timesteps.
MeanGridlock <- function(m){
  MeanGLs <- c()
  for(i in 1:ncol(m)){
    GLMean <- mean(m[m[,i]!=1000,i])
    MeanGLs <- cbind(MeanGLs,GLMean)
    colnames(MeanGLs)[[i]] <- c(paste("p=",probs[[i]], sep=""))
    rownames(MeanGLs)[[1]] <- c("Average GL Timesteps")
  }
  return(MeanGLs)
}

## Returns a matrix that lists the frequency that a certain car density in the traffic model would
## hit gridlock and maintain free-flow

TSFreq <- function(m){
  Gridlock_Frequency <- c()
  Free_Flow_Frequency <- c()
  for(i in 1:ncol(m)){
    freqGL <- length(m[m[,i]!=1000,i])/nrow(m)
    Gridlock_Frequency <- cbind(Gridlock_Frequency,freqGL)
    freqFF <- length(m[m[,i]==1000,i])/nrow(m)
    Free_Flow_Frequency <- cbind(Free_Flow_Frequency,freqFF)
  }
  Freqs <- rbind(Gridlock_Frequency,Free_Flow_Frequency)
  for(i in 1:ncol(m)){
    colnames(Freqs)[i] <- c(paste("p=",probs[[i]],sep=""))
  }
  rownames(Freqs) <- c("Gridlock Frequency","Free Flow Frequency")
  return(Freqs)
}



## 10x10 Matrix
TenDataMat <- SimData(10,10)
TenDataMat
save(TenDataMat, file="TenDataMat.RData")

## 20x20 Matrix
TwentyDataMat <- SimData(20,20)
TwentyDataMat
save(TwentyDataMat, file="TwentyDataMat.RData")

## 50x50 Matrix
FiftyDataMat <- SimData(50,50)
FiftyDataMat
save(FiftyDataMat, file= "FiftyDataMat.RData")

## Return the average steps according to P (10 by 10)
Ave.Mean.10by10 <- MeanGridlock(TenDataMat)
Ave.Mean.10by10
save(Ave.Mean.10by10, file= "Ave.Mean.10by10.RData")
barplot(Ave.Mean.10by10, xlab="probs", ylab= "Average Steps Taken", main = "Average Steps Taken vs Probs 10*10")

##
TSFreq(TenDataMat)

## Return the average steps according to P (20 by 20)
Ave.Mean.20by20 <- MeanGridlock(TwentyDataMat)
Ave.Mean.20by20
save(Ave.Mean.20by20, file= "Ave.Mean.20by20.RData")
barplot(Ave.Mean.20by20, xlab="probs", ylab= "Average Steps Taken", main = "Average Steps Taken vs Probs 20*20")

##
TSFreq(TwentyDataMat)

## Return the average steps according to P (50 by 50)
Ave.Mean.50by50 <- MeanGridlock(TenDataMat)
Ave.Mean.50by50
save(Ave.Mean.50by50, file= "Ave.Mean.50by50.RData")
barplot(Ave.Mean.50by50, xlab="probs", ylab= "Average Steps Taken", main = "Average Steps Taken vs Probs 50*50")

##
##
TSFreq(FiftyDataMat)
