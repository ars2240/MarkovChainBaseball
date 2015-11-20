#plays half of a game of baseball (one team batting) based on a list of 9 transition matrices
game <- function(tm){

  #starting conditions
  batter <- 1
  inning <- 1
  state <- 0
  runs <- 0

  #count number of outs
  outs <- floor(state/8)

  #count number of runners
  runners <- 0
  if(state %% 2==1){
    runners <- runners+1
  }
  if(state %% 4==2){
    runners <- runners+1
  }
  if(state %% 8==4){
    runners <- runners+1
  }

  while(inning<=9){
    #record number of outs plus runners prior to plate appearance
    oldOPR <- outs+runners

    #generate a random number
    rand <- runif(1,0,1)

    #split up number line between 0 and 1 into different outcomes
    sums <- c(NA)
    for(n in 1:25){
      sums[n] <- 1-sum(tm[[batter]][state+1,1:n], na.rm=T)
      if(tm[[batter]][state+1,n]==0){
        sums[n] <- NA
      }
    }

    #figure out what state the inning transitions to
    stateNew <- min(which(sums < rand))-1

    #get new number of outs and runners
    outs <- floor(stateNew/8)
    runners <- 0
    if(stateNew %% 2==1){
      runners <- runners+1
    }
    if(stateNew %% 4==2){
      runners <- runners+1
    }
    if(stateNew %% 8==4){
      runners <- runners+1
    }

    #if inning is not over, record number of runs scored
    if(stateNew!=24){
      runs <- runs + oldOPR - outs - runners + 1
    }

    #cycle batter and states
    batter <- batter+1
    state <- stateNew

    #if batter is >9, reset batting order
    if(batter>9){
      batter <- 1
    }

    #if inning is over, cylce inning and reset state, outs, runners
    if(state==24){
      state <- 0
      inning <- inning+1
      outs <- 0
      runners <- 0
    }
  }

  #return number of runs scored
  return(runs)
}