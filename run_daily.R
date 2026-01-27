source("R/match-function.R")
library(rvest)
library(stringr)
library(dplyr)
library(jsonlite)
library(oddsapiR)
library(lubridate)
library(tibble)
library(httr)



#first pull from kp basically just using PullStats2


date <- format(Sys.Date(),"%Y-%m-%d")

TdSpr <- toa_sports_odds(
  sport_key = 'basketball_ncaab',
  regions = "us",
  markets = "spreads",
  odds_format = "decimal",
  date_format = "iso"
)
FandSpr <- TdSpr[which(TdSpr$bookmaker == "FanDuel"),]

#FandSpr$commence_time <- with_tz(FandSpr$commence_time, tzone = "America/New_York")

HomeSpr <- FandSpr[seq(1,nrow(FandSpr),by=2),]

HomeSpr$commence_time <- ymd_hms(HomeSpr$commence_time, tz="UTC")

HomeSpr$commence_time <- with_tz(HomeSpr$commence_time, tzone = "America/New_York")

#DashDate <-format(Sys.time(), tz = "America/New_York", usetz = FALSE, "%Y-%m-%d")

HomeSpr <- HomeSpr[which(substr(HomeSpr$commence_time,1,10) == date),]


#make sure spread is home teams spread
if(nrow(HomeSpr) > 0){
for (i in 1:nrow(HomeSpr)){
  if (HomeSpr$outcomes_name[i] == HomeSpr$away_team[i]){
    if (substring(HomeSpr$outcomes_point[i],1,1) == "-"){
      HomeSpr$outcomes_point[i] <- paste0("+",substring(HomeSpr$outcomes_point[i],2,nchar(HomeSpr$outcomes_point[i])))
    } else{
      HomeSpr$outcomes_point[i] <- paste0("-",HomeSpr$outcomes_point[i])
    }
  }
}
}else{
  HomeSpr <- FandSpr[-(1:nrow(FandSpr)),]
}
names(HomeSpr)[names(HomeSpr) == "outcomes_point"] <- "home_spread"

#put in vector of oddsapi s, spits out kenpom 

#Homes <- HomeSpr$home_team

#names(HomeTeams) <- 'full_name'

#change to full_name for function
names(HomeSpr)[names(HomeSpr) == "home_team"] <- "full_name"

HomeSpr <- Teamtable(HomeSpr)

#and change back to home team
names(HomeSpr)[names(HomeSpr) == "full_name"] <- "home_team"

#repeat for away
names(HomeSpr)[names(HomeSpr) == "away_team"] <- "full_name"

HomeSpr <- Teamtable(HomeSpr)

names(HomeSpr)[names(HomeSpr) == "full_name"] <- "away_team"


HomeSpread <- HomeSpr[c(1,4:6,14)]


#repeat for total


TdTot <- toa_sports_odds(
  'basketball_ncaab',
  regions = "us",
  markets = "totals",
  odds_format = "american",
  date_format = "iso"
)

FandTot <- TdTot[which(TdTot$bookmaker == "FanDuel"),]

HomeTot <- FandTot[seq(1,nrow(FandTot),by=2),]

HomeTot$commence_time <- ymd_hms(HomeTot$commence_time, tz="UTC")

HomeTot$commence_time <- with_tz(HomeTot$commence_time, tzone = "America/New_York")

HomeTot <- HomeTot[which(substr(HomeTot$commence_time,1,10)==date),]

GamesTrack <- function(HomeTot,HomeSpr){
  
  names(HomeTot)[names(HomeTot) == "outcomes_point"] <- "home_total"
 
  #put in vector of oddsapi teams, spits out kenpom team
  
  #HomeTeams <- HomeSpr$home_team
  
  #names(HomeTeams) <- 'full_name'
  
  #change to full_name for function
  names(HomeTot)[names(HomeTot) == "home_team"] <- "full_name"
  
  HomeTot <- Teamtable(HomeTot)
  
  #and change back to home team
  names(HomeTot)[names(HomeTot) == "full_name"] <- "home_team"
  
  #repeat for away
  names(HomeTot)[names(HomeTot) == "away_team"] <- "full_name"
  
  HomeTot <- Teamtable(HomeTot)
  
  names(HomeTot)[names(HomeTot) == "full_name"] <- "away_team"
  
  
  HomeTotal <- HomeTot[c(1,4:6,14)]
  
  
  
  

  target_url <- "https://kenpom.com/"

  proxy_url <- paste0("http://api.scraperapi.com?api_key=", "0cb854720319f36ef4c5e17a9cf4bd57", "&url=", target_url)

  response <- GET(proxy_url)

  page <- read_html(response)

  
  Ranks <- page %>%
    html_nodes(".hard_left") %>%
    html_text()
  
  Teams <- page %>%
    html_nodes(".next_left") %>%
    html_text()
  
  Teams <- Teams[Teams != "" & Teams != "Team"]
  Ranks <- Ranks[Ranks != "" & Ranks != "Rk"]
  
  
  teamdat2 <- page %>%
    html_nodes(".td-left") %>%
    html_text()
  
  
  #teamdat2 goes in order of kenpom
  teammat2 <- matrix(teamdat2, ncol = 8, byrow = TRUE)
  
  colnames(teammat2) <- c("AdjO",
                          "AdjD",
                          "AdjT",
                          "Luck",
                          "NetSoS",
                          "ORtgSoS",
                          "DRtgSoS",
                          "NonConSOS")
  
  df <- as.data.frame(cbind(Ranks, Teams, teammat2))
  
  colnames(df)[2] <- "home_team"
  
  TodaysFullStat <- inner_join(HomeTotal, HomeSpread, by = c("id", "commence_time","home_team","away_team"))
  
  
  dfh <- inner_join(df,TodaysFullStat, by = "home_team")
  
  colnames(df)[2] <- "away_team"
  
  dfh2 <- inner_join(df,TodaysFullStat, by = "away_team")
  
  dfh3 <- inner_join(dfh,dfh2, by = c("id","home_team","away_team","commence_time","home_total","home_spread"))
  
  df_sorted <- dfh3[order(dfh$home_team), ]
  
  dfh4 <- dfh3[c(11,13,2,1,3:10,16:24)]
  
  names(dfh4) <- c("id","Away","Home",
                   "Ranks.H",
                   "AdjO.H",
                   "AdjD.H",
                   "AdjT.H",
                   "Luck.H",
                   "NetSoS.H",
                   "ORtgSoS.H",
                   "DRtgSoS.H",
                   "NonConSOS.H",
                   "Ranks.A",
                   "AdjO.A",
                   "AdjD.A",
                   "AdjT.A",
                   "Luck.A",
                   "NetSoS.A",
                   "ORtgSoS.A",
                   "DRtgSoS.A",
                   "NonConSOS.A")
  
  vegasodds <- dfh3[c(14,15)]
  
  combined_stat <- cbind(dfh4,vegasodds)
  
  #get rid of rank, its a kp rank so idc
  
  combined_stat <- combined_stat[,-4]
  
  
  #add score columns, model currently uses score.1
  
  VegasRes <- vector(length=nrow(combined_stat))
  combined_stat <- add_column(combined_stat, Score.H = NA, .after = "Home")
  combined_stat <- add_column(combined_stat, Score.A = NA, .after = "Score.H")
  combined_stat <- cbind(combined_stat,date,VegasRes)
  
  # i think we prob need to add combined_stat into a df that doesnt change like an rds file, then we can inner join from yesterday
  
  #saveRDS(NULL, "HistScoresGit.rds")
  
  Scores_file <- "HistScoresGit.rds"
  
  #so now we need two things, to build this rds with todays combined stat, then also inner join on toa daysfrom = 1
  
  HistGames <- readRDS(Scores_file)
  
  if (length(HistGames$date == date) == nrow(combined_stat)){
    AllGames <- HistGames
  } else {
    notin <- which(!combined_stat$id %in% HistGames$id)
    AllGames <- rbind(HistGames,combined_stat[notin,])
  }
  
  #saveRDS(AllGames, "HistScoresGit.rds")
  
  #index for what yesterdays games were
  ydgameindex <- which(AllGames$date == format(Sys.Date()-1, "%Y-%m-%d"))
  ydgames <- AllGames[ydgameindex,]
  
  
  #fake id
  #combined_stat[1,1] <- "9bf4dcafab589ee088bf1f1079e59143"
  
  #pulls scores from yesterday, if yesterday is two days ago(comes from weird utc timing)
  
  if(as.numeric(format(with_tz(now(), "America/New_York", "%Y-%m-%d"),"%H"))>=16){
    score_hist <- toa_sports_scores('basketball_ncaab',days_from = 2)
  }else{
    score_hist <- toa_sports_scores('basketball_ncaab',days_from = 1)
  }
  
  score_hist <- score_hist[,c(1,6,7,8)]
  
  score_hist <- add_column(score_hist, Score.H = NA, .after = "scores")
  score_hist <- add_column(score_hist, Score.A = NA, .after = "Score.H")
  
  
  #because whoever makes this is fucking lazy, we need to call match function again

  for(i in 1:length(which(sapply(score_hist$scores,is.null)==FALSE))){
    score_hist$Score.H[i] <- score_hist$scores[[i]][1,2]
    score_hist$Score.A[i] <- score_hist$scores[[i]][2,2]
  }

  
  
  ydscore <- score_hist[,c(1,5,6)]
  
  HistSc <- inner_join(ydgames, ydscore, by = c("id"))
  
  HistSc$Score.A.x <- HistSc$Score.A.y
  HistSc$Score.H.x <- HistSc$Score.H.y
  
  HistSc <- HistSc[,1:(ncol(HistSc)-1)]
  
  if(length(ydgameindex)==nrow(HistSc)){
    AllGames[ydgameindex,] <- HistSc
  } else{
    tempindex <- which(AllGames[,1] %in% HistSc[,1])
    AllGames[tempindex,] <- HistSc
  }
  
  #AllGames <- cbind(AllGames,"Vegas Result")
  
  names(AllGames)[[26]] <- "VegasRes"
  
  #AllGames$`"VegasRes"` <- as.numeric(AllGames$Score.H) - as.numeric(AllGames$Score.A)
  
  
  for(i in 1:nrow(AllGames)){
    
    if(is.na(AllGames$Score.H[i]) == FALSE){
      if(as.numeric(AllGames$home_spread[i]) < 0){
        if((as.numeric(AllGames$Score.H[i]) - as.numeric(AllGames$Score.A[i])) < 0){
          AllGames$VegasRes[i] = "Loss"
        }else if((as.numeric(AllGames$Score.H[i]) - as.numeric(AllGames$Score.A[i])) > 0){
          if(abs((as.numeric(AllGames$Score.H[i]) - as.numeric(AllGames$Score.A[i]))) <= as.numeric(AllGames$home_spread[i])){
            AllGames$VegasRes[i] = "Loss" 
          }else{
            AllGames$VegasRes[i] = "Hit" 
          }
        }
      } else if(as.numeric(AllGames$home_spread[i]) >= 0){
        if((as.numeric(AllGames$Score.H[i]) - as.numeric(AllGames$Score.A[i])) > 0){
          AllGames$VegasRes[i] = "Hit"
        } else{
          if(abs(as.numeric(AllGames$Score.H[i]) - as.numeric(AllGames$Score.A[i])) > as.numeric(AllGames$home_spread[i])){
            AllGames$VegasRes[i] = "Hit" 
          }else{
            AllGames$VegasRes[i] = "Loss"
          }
        }
        
        
      }
    }  
    
    
  }
  return(AllGames)
}

#make sure spread is home teams spread
if(nrow(HomeTot) > 0){
for (i in 1:nrow(HomeTot)){
  if (HomeTot$outcomes_name[i] == HomeTot$away_team[i]){
    if (substring(HomeTot$outcomes_point[i],1,1) == "-"){
      HomeTot$outcomes_point[i] <- paste0("+",substring(HomeTot$outcomes_point[i],2,nchar(HomeTot$outcomes_point[i])))
    } else{
      HomeTot$outcomes_point[i] <- paste0("-",HomeTot$outcomes_point[i])
    }
  }
}
AllGames <- GamesTrack(HomeTot,HomeSpr)
}else{
 Scores_file <- "HistScoresGit.rds"
 AllGames <- readRDS(Scores_file) 
}


#AllGames <- AllGames[,1:25]


saveRDS(AllGames, "HistScoresGit.rds")


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

#all games, some dont have scores 
AllGameandPred <- PredScore(AllGames)


#AllGame andPred2 has all the games and predictions with a score that was tracked
AllGameandPred2 <- AllGameandPred[-which(is.na(AllGameandPred$PredAway) == TRUE),]

AllGameandPred2$MySpread <- AllGameandPred2$PredAway - AllGameandPred2$PredHome
AllGameandPred2$MyTot <- AllGameandPred2$PredHome + AllGameandPred2$PredAway


ID <- vector()
HitRate <- vector()
NumPlays <- vector()

for(j in 0:80){

sprmeasure <- as.numeric(j*.1)
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
  } else if((as.numeric(AllGameandPred2$Score.H[i]) + as.numeric(AllGameandPred2$home_spread[i])) <= AllGameandPred2$Score.A[i]){
    if(AllGameandPred2$SpreadPlay[i] == "Home Spread"){
      AllGameandPred2$SpreadAcc[i] <-  "Miss"
    }else if(AllGameandPred2$SpreadPlay[i] == "Away Spread"){
      AllGameandPred2$SpreadAcc[i] <- "Hit"
    }else{
      AllGameandPred2$SpreadAcc[i] <- NA
    }
  }
  
}


#sets up total play and total accuary
for(i in 1:nrow(AllGameandPred2)){
  totmeasure <- 8.5
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

HitRate[j] <- (TotHit + SprHit) /(TotMiss + SprMiss + TotHit + SprHit)
NumPlays[j] <- ((TotMiss + SprMiss + TotHit + SprHit))
ID[j] <- j


#CombStats$ID[i] <- i 
#CombStats$HitRate[i] <- HitRate
#CombStats$NumPlays[i] <- NumPlays
}
CombStats <- as.data.frame(cbind(ID,HitRate,NumPlays))
saveRDS(CombStats, "SprStats.rds")
