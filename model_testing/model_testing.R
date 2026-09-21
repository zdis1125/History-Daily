dat25 <- read.csv("CBB_data.csv", skip = 2)
dat25 <- dat25[-nrow(dat25), ]
dat25$AdjO.A <- as.numeric(dat25$AdjO.A)
dat25$AdjD.A <- as.numeric(dat25$AdjD.A)
dat25$AdjT.A <- as.numeric(dat25$AdjT.A)
dat25$AdjT.H <- as.numeric(dat25$AdjT.H)
dat25$AdjO.H <- as.numeric(dat25$AdjO.H)
dat25$AdjD.H <- as.numeric(dat25$AdjD.H)

names(dat25)
fit1 <- lm(score ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H, data = dat25)


HomePred <- lm(score.1 ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H, data = dat25)
AwayPred <- lm(score ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H, data = dat25)

"summary(HomePred)
summary(AwayPred)
"


PredScore <- function(GamesHist){
  GamesHist$PredHome <- NA
  GamesHist$PredAway <- NA
  NonNA <- which(is.na(GamesHist$Score.H) == FALSE)
  for(i in NonNA){
    new_game = data.frame( AdjO.A = as.numeric(GamesHist$AdjO.A[i]),
                           AdjD.A = as.numeric(GamesHist$AdjD.A[i]),
                           AdjT.A = as.numeric(GamesHist$AdjT.A[i]),
                           AdjO.H = as.numeric(GamesHist$AdjO.H[i]),
                           AdjD.H = as.numeric(GamesHist$AdjD.H[i]),
                           AdjT.H = as.numeric(GamesHist$AdjT.H[i])
    )
    GamesHist$PredHome[i] <- round(predict(HomePred, new_game),1)
    GamesHist$PredAway[i] <- round(predict(AwayPred, new_game),1)
  }
  return(GamesHist)
}

#Flipped predictions, for non conference, remember this just flips the models, home now uses away model, but is still a home prediction
FlipPredScore <- function(GamesHist){
}

#all games, some dont have scores 
AllGameandPred <- PredScore(AllGames)

FlipGameandPred <- FlipPredScore(AllGames)

#remove for Non Con, this averages the normal and flipped predictions for a non con estimate
#for(i in 1:nrow(AllGameandPred)){
#AllGameandPred$PredHome[i] <- (AllGameandPred$PredHome[i] + FlipGameandPred$PredHome[i]) / 2
#AllGameandPred$PredAway[i] <- (AllGameandPred$PredAway[i] + FlipGameandPred$PredAway[i]) / 2
#}


#AllGame andPred2 has all the games and predictions with a score that was tracked
AllGameandPred2 <- AllGameandPred[-which(is.na(AllGameandPred$PredAway) == TRUE),]

AllGameandPred2$MySpread <- AllGameandPred2$PredAway - AllGameandPred2$PredHome
AllGameandPred2$MyTot <- AllGameandPred2$PredHome + AllGameandPred2$PredAway


HitRate <- matrix(ncol = 125,nrow = 80)
NumPlays <- matrix(ncol = 125,nrow = 80)
ID <- matrix(ncol = 125,nrow = 80)
for( k in 1:80) {
  
    sprmeasure <- k/10
    for(i in 1:nrow(AllGameandPred2)){
      AllGameandPred2$SpreadDiscrep[i] <- abs(AllGameandPred2$MySpread[i]- as.numeric(AllGameandPred2$home_spread[i]))
      if(abs(AllGameandPred2$SpreadDiscrep[i] >=sprmeasure)){
        if(abs(AllGameandPred2$MySpread[i] + AllGameandPred2$SpreadDiscrep[i]) == abs(as.numeric(AllGameandPred2$home_spread[i]))){
          AllGameandPred2$SpreadPlay[i] <- "Home Spread"
        } else if(abs(AllGameandPred2$MySpread[i] - AllGameandPred2$SpreadDiscrep[i]) == abs(as.numeric(AllGameandPred2$home_spread[i]))){
          AllGameandPred2$SpreadPlay[i] <- "Away Spread" 
        }else{
          AllGameandPred2$SpreadPlay[i] <- "Nah" 
        }
      }else{
        AllGameandPred2$SpreadPlay[i] <- "No"
      }
      
      #AllGameandPred2$SpreadAcc[i] <-  as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$home_spread[i])
      
      if((as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$home_spread[i])) >= AllGameandPred2$Score.A[i]){
        if(AllGameandPred2$SpreadPlay[i] == "Away Spread"){
          AllGameandPred2$SpreadAcc[i] <-  "Miss"
        }else if(AllGameandPred2$SpreadPlay[i] == "Home Spread"){
          AllGameandPred2$SpreadAcc[i] <- "Hit"
        }else{
          AllGameandPred2$SpreadAcc[i] <- NA
        }
      } else if((as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$home_spread[i])) < AllGameandPred2$Score.A[i]){
        if(AllGameandPred2$SpreadPlay[i] == "Home Spread"){
          AllGameandPred2$SpreadAcc[i] <-  "Miss"
        }else if(AllGameandPred2$SpreadPlay[i] == "Away Spread"){
          AllGameandPred2$SpreadAcc[i] <- "Hit"
        }else{
          AllGameandPred2$SpreadAcc[i] <- NA
        }
      }
      
    }
    
    for(j in 1:125){
    #sets up total play and total accuary
    for(i in 1:nrow(AllGameandPred2)){
      totmeasure <- j/10
      if(AllGameandPred2$home_total[i] - AllGameandPred2$MyTot[i] >= totmeasure){
        AllGameandPred2$TotalPlay[i] <- "Under"
      } else if(AllGameandPred2$home_total[i] - AllGameandPred2$MyTot[i] <= -(totmeasure)){
        AllGameandPred2$TotalPlay[i] <- "Over"
      } else{
        AllGameandPred2$TotalPlay[i] <- "No"
      }
      
      #AllGameandPred2$totdiscrep[i] <- abs(AllGameandPred2$home_total[i] - AllGameandPred2$MyTot[i])
      if((as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$Score.A[i])) >= AllGameandPred2$home_total[i]){
        if(AllGameandPred2$TotalPlay[i] == "Over"){
          AllGameandPred2$TotalAcc[i] <-  "Hit"
        }else if(AllGameandPred2$TotalPlay[i] == "Under"){
          AllGameandPred2$TotalAcc[i] <- "Miss"
        }else{
          AllGameandPred2$TotalAcc[i] <- NA
        }
      } else if((as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$Score.A[i])) <= AllGameandPred2$home_total[i]){
        if(AllGameandPred2$TotalPlay[i] == "Under"){
          AllGameandPred2$TotalAcc[i] <-  "Hit"
        }else if(AllGameandPred2$TotalPlay[i] == "Over"){
          AllGameandPred2$TotalAcc[i] <- "Miss"
        }else{
          AllGameandPred2$TotalAcc[i] <- NA
        }
      }
    }
      TotHit <- length(which(AllGameandPred2$TotalAcc == "Hit"))
      TotMiss <- length(which(AllGameandPred2$TotalAcc == "Miss"))
      SprHit <- length(which(AllGameandPred2$SpreadAcc == "Hit"))
      SprMiss <- length(which(AllGameandPred2$SpreadAcc == "Miss"))
      
      HitRate[k,j] <- (TotHit + SprHit) /(TotMiss + SprMiss + TotHit + SprHit)
      NumPlays[k,j] <- ((TotMiss + SprMiss + TotHit + SprHit))
      ID[k,j] <- k + j
    }
    

}



fundval <- matrix(nrow = 80,ncol = 125)
i <- 1
for(i in 1:80){
  for(l in 1:125){
    fundval[i,l] <- 10 * HitRate[i,l] * NumPlays[i,l] - 11 * NumPlays[i,l] * (1- HitRate[i,l])
  }
}
