library(randomForest)
library(e1071)
library(gbm)
# library(gbm) # Uncomment if you add gbm back

dat25 <- read.csv("CBB_data.csv", skip = 2)
dat25 <- dat25[-nrow(dat25), ]
#dat25$Score.H <- as.numeric(dat25$Score.H)
#dat25$Score.A <- as.numeric(dat25$Score.A)
dat25$AdjO.A <- as.numeric(dat25$AdjO.A)
dat25$AdjD.A <- as.numeric(dat25$AdjD.A)
dat25$AdjT.A <- as.numeric(dat25$AdjT.A)
dat25$AdjT.H <- as.numeric(dat25$AdjT.H)
dat25$AdjO.H <- as.numeric(dat25$AdjO.H)
dat25$AdjD.H <- as.numeric(dat25$AdjD.H)

Scores_file <- "HistScoresGit.rds"
AllGames <- readRDS(Scores_file)

# Convert prediction variables to numeric so they match the dat25 fit
AllGames$AdjO.A <- as.numeric(AllGames$AdjO.A)
AllGames$AdjD.A <- as.numeric(AllGames$AdjD.A)
AllGames$AdjT.A <- as.numeric(AllGames$AdjT.A)
AllGames$AdjO.H <- as.numeric(AllGames$AdjO.H)
AllGames$AdjD.H <- as.numeric(AllGames$AdjD.H)
AllGames$AdjT.H <- as.numeric(AllGames$AdjT.H)

# Convert scores and Vegas lines to numeric for the accuracy loop
AllGames$Score.H <- as.numeric(AllGames$Score.H)
AllGames$Score.A <- as.numeric(AllGames$Score.A)
AllGames$home_spread <- as.numeric(AllGames$home_spread)
AllGames$home_total <- as.numeric(AllGames$home_total)

model_formulaA = score ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H
model_formulaH = score.1 ~ AdjD.A + AdjO.H + AdjT.A + AdjT.H  + AdjO.A + AdjD.H

features <- c("AdjD.A", "AdjO.H", "AdjT.A",
              "AdjT.H", "AdjO.A", "AdjD.H")

train_x <- dat25[, features, drop = FALSE]
train_x[] <- lapply(train_x, as.numeric)

home_y <- as.numeric(dat25$score.1)
away_y <- as.numeric(dat25$score)

keep_home <- complete.cases(train_x, home_y)
keep_away <- complete.cases(train_x, away_y)

model_list <- list(
  #Linear_Regression = function(f, data) lm(f, data = data),
  #Random_Forest     = function(f, data) randomForest(f, data = data, ntree = 500, mtry = 3),
  #Random_Forest2     = function(f, data) randomForest(f, data = data, ntree = 100, mtry = 3),
  #Random_Forest3     = function(f, data) randomForest(f, data = data, ntree = 500, mtry = 2),
  #Random_Forest4     = function(f, data) randomForest(f, data = data, ntree = 100, mtry = 2),
  #Support_Vector1    = function(f, data) svm(f, data = data, cost = 10, gamma = 0.1),
  svm1    = function(x,y) svm(x, y, type = "eps-regression",kernel = "radial", cost = 100, gamma =.1, epsilon = 1)
  #SVM_Radial        = function(f, data) svm(f, data = data, kernel = "radial", cost = 10),
  #gbm = function(f,data) gbm(f,data = data, n.trees= 10000,interaction.depth = 1, shrinkage = 0.01),
  #gbm2 = function(f,data) gbm(f,data = data, n.trees= 10000,interaction.depth = 3, shrinkage = 0.01),
  #gbm3 = function(f,data) gbm(f,data = data, n.trees= 20000,interaction.depth = 1, shrinkage = 0.01),
  #gbm4 = function(f,data) gbm(f,data = data, n.trees= 10000,interaction.depth = 1, shrinkage = 0.05),
  #gbm5 = function(f,data) gbm(f,data = data, n.trees= 500,interaction.depth = 12, shrinkage = 0.0001),
  #glm_Gam_log               = function(f, data) glm(f, data = data, family = Gamma(link = "log")),
  #glm_Gam_ident              = function(f, data) glm(f, data = data, family = Gamma(link = "identity")),
  #glm_Gau_log               = function(f, data) glm(f, data = data, family = gaussian(link = "log"))
)

model_results <- data.frame()

for (model_name in names(model_list)) {
  
  fit_func <- model_list[[model_name]]
  if(grepl("svm", model_name))
    HomePred <- fit_func(model_formulaH, x = train_x[keep_home, , drop = FALSE], y = home_y[keep_home])
    AwayPred <- fit_func(model_formulaA, x = train_x[keep_home, , drop = FALSE], y = away_y[keep_home])
  else{
    HomePred <- fit_func(model_formulaH, data = dat25)
    AwayPred <- fit_func(model_formulaA, data = dat25)
  }
  
  # --- OPTIMIZED PREDICTION FUNCTION ---
  PredScore <- function(GamesHist){
    GamesHist$PredHome <- NA
    GamesHist$PredAway <- NA
    
    NonNA <- which(!is.na(GamesHist$Score.H))
    
    # Vectorized prediction is much faster than row-by-row. 
    # type="response" ensures GLM predictions are returned as actual scores, not log values.
    # Check if the current model in the loop is a GBM
    if(grepl("gbm", model_name)) {
      # gbm requires n.trees for prediction
      GamesHist$PredHome[NonNA] <- round(predict(HomePred, newdata = GamesHist[NonNA, ], n.trees = HomePred$n.trees, type = "response"), 1)
      GamesHist$PredAway[NonNA] <- round(predict(AwayPred, newdata = GamesHist[NonNA, ], n.trees = AwayPred$n.trees, type = "response"), 1)
    } else if(grepl("svm", model_name)) {
      GamesHist$PredHome[NonNA] <- round(predict(HomePred, newdata = GamesHist[NonNA, features, drop = FALSE]), 1)
      GamesHist$PredAway[NonNA] <- round(predict(AwayPred, newdata = GamesHist[NonNA, features, drop = FALSE]), 1)
    } else {
      # Standard prediction for lm, glm, randomForest, svm
      GamesHist$PredHome[NonNA] <- round(predict(HomePred, newdata = GamesHist[NonNA, ], type = "response"), 1)
      GamesHist$PredAway[NonNA] <- round(predict(AwayPred, newdata = GamesHist[NonNA, ], type = "response"), 1)
    }
    
    #GamesHist$PredHome[NonNA] <- round(predict(HomePred, newdata = GamesHist[NonNA, ], type = "response"), 1)
    #GamesHist$PredAway[NonNA] <- round(predict(AwayPred, newdata = GamesHist[NonNA, ], type = "response"), 1)
    
    return(GamesHist)
  }
  
  AllGameandPred <- PredScore(AllGames)
  
  # Filter out games without predictions
    # Keep games with both predictions and both final scores
  AllGameandPred2 <- AllGameandPred[
    complete.cases(
      AllGameandPred[, c("PredHome", "PredAway", "Score.H", "Score.A")]
    ),
    , drop = FALSE
  ]

  AllGameandPred2$MySpread <- round(
    AllGameandPred2$PredAway - AllGameandPred2$PredHome, 1
  )

  AllGameandPred2$MyTot <- round(
    AllGameandPred2$PredHome + AllGameandPred2$PredAway, 1
  )

  # Positive spread edge = Home; negative = Away
  AllGameandPred2$SpreadEdge <- round(
    AllGameandPred2$home_spread - AllGameandPred2$MySpread, 1
  )

  AllGameandPred2$SpreadDiscrep <- abs(AllGameandPred2$SpreadEdge)

  # Positive total edge = Over; negative = Under
  AllGameandPred2$TotalEdge <- round(
    AllGameandPred2$MyTot - AllGameandPred2$home_total, 1
  )

  # Positive margins mean Home covered / total went Over
  spread_margin <- AllGameandPred2$Score.H +
    AllGameandPred2$home_spread - AllGameandPred2$Score.A

  total_margin <- AllGameandPred2$Score.H +
    AllGameandPred2$Score.A - AllGameandPred2$home_total

  # Grade the margin from the selected side's perspective
  grade_pick <- function(selected, margin) {
    result <- rep(NA_character_, length(margin))
    valid <- selected & is.finite(margin)

    result[valid] <- ifelse(
      abs(margin[valid]) < 1e-8,
      "Push",
      ifelse(margin[valid] >= 0, "Hit", "Miss")
    )

    result
  }

  HitRate <- matrix(NA_real_, nrow = 80, ncol = 125)
  NumPlays <- matrix(0, nrow = 80, ncol = 125)
  ID <- matrix(0, nrow = 80, ncol = 125)
  fundval <- matrix(0, nrow = 80, ncol = 125)

  for (k in 1:80) {
    sprmeasure <- k / 10

    # Strictly greater than the spread threshold
    spread_selected <- is.finite(AllGameandPred2$SpreadEdge) &
      AllGameandPred2$SpreadDiscrep >= sprmeasure

    AllGameandPred2$SpreadPlay <- ifelse(
      spread_selected,
      ifelse(AllGameandPred2$SpreadEdge >= 0,
             "Home Spread", "Away Spread"),
      "No"
    )

    # Multiply by the edge's sign to grade the chosen side
    AllGameandPred2$SpreadAcc <- grade_pick(
      spread_selected,
      spread_margin * sign(AllGameandPred2$SpreadEdge)
    )

    SprHit <- sum(AllGameandPred2$SpreadAcc == "Hit", na.rm = TRUE)
    SprMiss <- sum(AllGameandPred2$SpreadAcc == "Miss", na.rm = TRUE)

    for (j in 1:125) {
      totmeasure <- j / 10

      # Strictly greater than the total threshold
      total_selected <- is.finite(AllGameandPred2$TotalEdge) &
        abs(AllGameandPred2$TotalEdge) >= totmeasure

      AllGameandPred2$TotalPlay <- ifelse(
        total_selected,
        ifelse(AllGameandPred2$TotalEdge >= 0, "Over", "Under"),
        "No"
      )

      AllGameandPred2$TotalAcc <- grade_pick(
        total_selected,
        total_margin * sign(AllGameandPred2$TotalEdge)
      )

      TotHit <- sum(AllGameandPred2$TotalAcc == "Hit", na.rm = TRUE)
      TotMiss <- sum(AllGameandPred2$TotalAcc == "Miss", na.rm = TRUE)

      wins <- SprHit + TotHit
      losses <- SprMiss + TotMiss

      # Preserve your original definition: wins + losses
      # Pushes are excluded from play count and hit rate
      NumPlays[k, j] <- wins + losses

      HitRate[k, j] <- if (wins + losses > 0) {
        wins / (wins + losses)
      } else {
        NA_real_
      }

      ID[k, j] <- k + j

      # Preserve your original fund-value scale
      # +10 per win, -11 per loss, 0 per push
      fundval[k, j] <- 10 * wins - 11 * losses
    }
  }
    max_fund <- max(fundval, na.rm = TRUE)

  # Keep all coordinates if multiple threshold pairs tie for the maximum
  locations <- which(fundval == max_fund, arr.ind = TRUE)

  model_results <- rbind(
    model_results,
    data.frame(
      Model = model_name,
      MaxFundValue = max_fund,
      NetUnits = max_fund / 10,
      Row = locations[, "row"],
      Column = locations[, "col"],
      SpreadThreshold = locations[, "row"] / 10,
      TotalThreshold = locations[, "col"] / 10
    )
  )
  # Save grids for this specific model
  #saveRDS(as.data.frame(HitRate), paste0("model_testing/",model_name, "_HitRate_Grid.rds"))
  #saveRDS(as.data.frame(fundval), paste0("model_testing/",model_name, "_FundVal_Grid.rds"))
  #saveRDS(as.data.frame(NumPlays), paste0("model_testing/",model_name, "_NumPlays_Grid.rds"))
}


# Sort models from highest to lowest fund value
model_results <- model_results[
  order(model_results$MaxFundValue, decreasing = TRUE),
]

rownames(model_results) <- NULL

write.csv(model_results, "model_testing/model_results.csv", row.names = FALSE)

#View(model_results)

