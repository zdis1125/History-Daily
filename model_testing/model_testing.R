library(randomForest)
library(e1071)
# library(gbm) # Uncomment if you add gbm back

dat25 <- read.csv("CBB_data.csv", skip = 2)
dat25 <- dat25[-nrow(dat25), ]
dat25$AdjO.A <- as.numeric(dat25$AdjO.A)
dat25$AdjD.A <- as.numeric(dat25$AdjD.A)
dat25$AdjT.A <- as.numeric(dat25$AdjT.A)
dat25$AdjT.H <- as.numeric(dat25$AdjT.H)
dat25$AdjO.H <- as.numeric(dat25$AdjO.H)
dat25$AdjD.H <- as.numeric(dat25$AdjD.H)

Scores_file <- "HistScoresGit.rds"
AllGames <- readRDS(Scores_file)

model_formulaA = score ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H
model_formulaH = score.1 ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H

model_list <- list(
  Linear_Regression = function(f, data) lm(f, data = data),
  #Random_Forest     = function(f, data) randomForest(f, data = data, ntree = 5000, mtry = 3),
  Support_Vector    = function(f, data) svm(f, data = data, cost = 10, gamma = 0.1),
  #SVM_Radial        = function(f, data) svm(f, data = data, kernel = "radial", cost = 10),
  #glm               = function(f, data) glm(f, data = data, family = Gamma(link = "log"))
)

for (model_name in names(model_list)) {
  
  fit_func <- model_list[[model_name]]
  HomePred <- fit_func(model_formulaH, data = dat25)
  AwayPred <- fit_func(model_formulaA, data = dat25)
  
  # --- OPTIMIZED PREDICTION FUNCTION ---
  PredScore <- function(GamesHist){
    GamesHist$PredHome <- NA
    GamesHist$PredAway <- NA
    
    NonNA <- which(!is.na(GamesHist$Score.H))
    
    # Vectorized prediction is much faster than row-by-row. 
    # type="response" ensures GLM predictions are returned as actual scores, not log values.
    GamesHist$PredHome[NonNA] <- round(predict(HomePred, newdata = GamesHist[NonNA, ], type = "response"), 1)
    GamesHist$PredAway[NonNA] <- round(predict(AwayPred, newdata = GamesHist[NonNA, ], type = "response"), 1)
    
    return(GamesHist)
  }
  
  AllGameandPred <- PredScore(AllGames)
  
  # Filter out games without predictions
  AllGameandPred2 <- AllGameandPred[!is.na(AllGameandPred$PredAway), ]
  
  AllGameandPred2$MySpread <- AllGameandPred2$PredAway - AllGameandPred2$PredHome
  AllGameandPred2$MyTot <- AllGameandPred2$PredHome + AllGameandPred2$PredAway
  
  # --- REQUIRED PRE-CALCULATIONS FOR VECTORIZED LOOP ---
  AllGameandPred2$home_spread <- as.numeric(AllGameandPred2$home_spread)
  AllGameandPred2$Score.H <- as.numeric(AllGameandPred2$Score.H)
  AllGameandPred2$Score.A <- as.numeric(AllGameandPred2$Score.A)
  
  AllGameandPred2$SpreadDiscrep <- abs(AllGameandPred2$MySpread - AllGameandPred2$home_spread)
  home_covered <- (AllGameandPred2$Score.H + AllGameandPred2$home_spread) >= AllGameandPred2$Score.A
  away_covered <- (AllGameandPred2$Score.H + AllGameandPred2$home_spread) < AllGameandPred2$Score.A
  
  HitRate <- matrix(ncol = 125, nrow = 80)
  NumPlays <- matrix(ncol = 125, nrow = 80)
  ID <- matrix(ncol = 125, nrow = 80)
  
  for( k in 1:80) {
    sprmeasure <- k/10
    
    is_home_play <- round(abs(AllGameandPred2$MySpread + AllGameandPred2$SpreadDiscrep), 4) == round(abs(AllGameandPred2$home_spread), 4)
    is_away_play <- round(abs(AllGameandPred2$MySpread - AllGameandPred2$SpreadDiscrep), 4) == round(abs(AllGameandPred2$home_spread), 4)
    
    AllGameandPred2$SpreadPlay <- ifelse(AllGameandPred2$SpreadDiscrep >= sprmeasure,
                                         ifelse(is_home_play, "Home Spread",
                                                ifelse(is_away_play, "Away Spread", "Nah")),
                                         "No")
    
    AllGameandPred2$SpreadAcc <- ifelse(home_covered & AllGameandPred2$SpreadPlay == "Home Spread", "Hit",
                                        ifelse(home_covered & AllGameandPred2$SpreadPlay == "Away Spread", "Miss",
                                               ifelse(away_covered & AllGameandPred2$SpreadPlay == "Away Spread", "Hit",
                                                      ifelse(away_covered & AllGameandPred2$SpreadPlay == "Home Spread", "Miss", NA))))
    
    for(j in 1:125){
      totmeasure <- j/10
      
      diff_tot <- AllGameandPred2$home_total - AllGameandPred2$MyTot
      
      AllGameandPred2$TotalPlay <- ifelse(diff_tot >= totmeasure, "Under",
                                          ifelse(diff_tot <= -totmeasure, "Over", "No"))
      
      actual_tot <- AllGameandPred2$Score.H + AllGameandPred2$Score.A
      
      AllGameandPred2$TotalAcc <- ifelse(actual_tot >= AllGameandPred2$home_total & AllGameandPred2$TotalPlay == "Over", "Hit",
                                         ifelse(actual_tot <= AllGameandPred2$home_total & AllGameandPred2$TotalPlay == "Under", "Hit",
                                                ifelse(AllGameandPred2$TotalPlay %in% c("Over", "Under"), "Miss", NA)))
      
      # Tally Results
      TotHit <- sum(AllGameandPred2$TotalAcc == "Hit", na.rm = TRUE)
      TotMiss <- sum(AllGameandPred2$TotalAcc == "Miss", na.rm = TRUE)
      SprHit <- sum(AllGameandPred2$SpreadAcc == "Hit", na.rm = TRUE)
      SprMiss <- sum(AllGameandPred2$SpreadAcc == "Miss", na.rm = TRUE)
      
      HitRate[k,j] <- (TotHit + SprHit) / (TotMiss + SprMiss + TotHit + SprHit)
      NumPlays[k,j] <- (TotMiss + SprMiss + TotHit + SprHit)
      ID[k,j] <- k + j
    }
  }
  
  # Fundamental Value Calculation (-110 odds assumption)
  fundval <- matrix(nrow = 80, ncol = 125)
  for(i in 1:80){
    for(l in 1:125){
      fundval[i,l] <- 10 * HitRate[i,l] * NumPlays[i,l] - 11 * NumPlays[i,l] * (1 - HitRate[i,l])
    }
  }
  
  # Save grids for this specific model
  saveRDS(as.data.frame(HitRate), paste0(model_name, "_HitRate_Grid.rds"))
  saveRDS(as.data.frame(fundval), paste0(model_name, "_FundVal_Grid.rds"))
  saveRDS(as.data.frame(NumPlays), paste0(model_name, "_NumPlays_Grid.rds"))
}
