#create vectors for home runs and plate appearances
HR <- c(NA)
PA <- c(NA)

#split up events data by batter
ptm <- proc.time()
eventsByBatter <- split(events, events$BAT_ID)
print(paste0("Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))

#record the number of home runs and plate appearances for each batter
ptm <- proc.time()
for(n in 1:length(eventsByBatter)){
  temp <- subset(eventsByBatter[[n]], (sitNext==0 & sit <8) | (sitNext==8 & sit <16 & sit>=8) | (sitNext==16 & sit <24 & sit>=16)| (sitNext==24 & sit==0))
  HR[n] <- nrow(temp)
  PA[n] <- nrow(eventsByBatter[[n]])

  if(n %% floor(length(eventsByBatter)/20)==0){
    print(paste0(n/length(eventsByBatter),"% complete. Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))
  }
}
print(paste0("Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))

#get the top 5% of HR/PA --> sluggers
c <- quantile((HR/PA)[which(PA>100)],.95)

#create a list of sluggers
sluggers <- names(eventsByBatter)[which((HR/PA)>c & PA>100)]
eventsSLG <- subset(events, BAT_ID%in%sluggers)

#create transition matrix
tMatrixSLG <- matrix(0, ncol=25, nrow=25)
colnames(tMatrixSLG) <- 0:24
rownames(tMatrixSLG) <- 0:24

#get table of events, divide by number of events
tMatrixSLG[1:24,] <- table(eventsSLG[,c("sit","sitNext")])
tMatrixSLG[1:24,] <- tMatrixSLG[1:24,]/rowSums(tMatrixSLG[1:24,])

#save table as csv
write.csv(tMatrix,"~/Documents/Columbia/Applied Math Seminar/tMatrixSLG.csv")

#place transition matrix in all 9 batting slots
tm <- list(NA)
for(n in 1:9){
  tm[[n]] <- tMatrixSLG
}