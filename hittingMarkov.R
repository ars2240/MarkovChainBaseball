library(RMySQL)

#get events data from MySQL database
con <- dbConnect(dbDriver("MySQL"), user = "root", password = "****", dbname = "test")
events <- dbGetQuery(con, "select * from events")
dbDisconnect(con)

ptm <- proc.time()

# Situation ID = (# of outs) * 8 + (# of runners on 3rd) * 4 + (# of runners on 2nd) * 2 + (# of runners on 1st) * 1

# Examples:
# 0 is no outs, no men on
# 1 is no outs, runner on 1st
# 2 is no outs, runner on 2nd
# 3 is no outs, runners on 1st & 2nd
# …

events$sit <- events$OUTS_CT*8
events$sit[which(events$RUN1_RESP_PIT_ID!="")] <- events$sit[which(events$RUN1_RESP_PIT_ID!="")] + 1
events$sit[which(events$RUN2_RESP_PIT_ID!="")] <- events$sit[which(events$RUN2_RESP_PIT_ID!="")] + 2
events$sit[which(events$RUN3_RESP_PIT_ID!="")] <- events$sit[which(events$RUN3_RESP_PIT_ID!="")] + 4

#get next situation
events$sitNext <- NA
events$sitNext[1:(nrow(events)-1)] <- events$sit[2:(nrow(events))]

#change next situation for end of innings (change of batters or outs resetting)
events$batNext <- NA
events$batNext[1:(nrow(events)-1)] <- events$BAT_TEAM_ID[2:(nrow(events))]
events$sitNext[which(events$BAT_TEAM_ID!=events$batNext)] <- 24
events$sitNext[which(events$sitNext==0 & events$sit>=8)] <- 24

print(paste0("Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))

#create transition matrix
tMatrix <- matrix(0, ncol=25, nrow=25)
colnames(tMatrix) <- 0:24
rownames(tMatrix) <- 0:24

#get table of events, divide by number of events
ptm <- proc.time()
tMatrix[1:24,] <- table(events[,c("sit","sitNext")])
tMatrix[1:24,] <- tMatrix[1:24,]/rowSums(tMatrix[1:24,])
print(paste0("Time elapsed: ",floor((proc.time() - ptm)[3]/60)," min ",(proc.time() - ptm)[3]%%60," sec"))

#save table as csv
write.csv(tMatrix,"~/Documents/Columbia/Applied Math Seminar/tMatrix.csv")

#place transition matrix in all 9 batting slots
tm <- list(NA)
for(n in 1:9){
  tm[[n]] <- tMatrix
}