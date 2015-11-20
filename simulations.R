#place transition matrix in all 9 batting slots
tm <- list(NA)
for(n in 1:9){
  tm[[n]] <- tMatrix
  if(n==8){
    tm[[n]] <- tMatrixP
  }
}

#run 10,000 simulations of a baseball game and record runs scored
ptm <- proc.time()
nsim <- 10000
sims <- c(NA)
for(n in 1:nsim){
  sims[n] <- game(tm)
}
print(paste0("Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))

#print summary data of runs scored and display a histogram
print(summary(sims))
hist(sims, breaks=50, xlim=c(0,30), ylim=c(0,1250), xlab="Runs", main="Runs per Game - Pitcher Bats 8th")
