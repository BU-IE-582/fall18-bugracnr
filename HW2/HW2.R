require(data.table)
require(anytime)
require(dplyr)
library(rgl)

setwd("C:/IE_582_Rep/fall18-bugracnr/HW2")
getwd()

rm(list=ls())
gc()

#save paths
matches_file_path = "HW2_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_file_path = "HW2_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

#load data
matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)

matches[,match_time:=anytime(date)]
matches[,Year:=year(match_time)]
matches[,c("match_time","date","leagueId","type"):=NULL]

#Over Under & 1x2 Results
matches[,c("HomeGoals","AwayGoals"):=tstrsplit(score,':')]
matches$HomeGoals=as.numeric(matches$HomeGoals)
matches[,AwayGoals:=as.numeric(AwayGoals)]
matches[,TotalGoals:=HomeGoals+AwayGoals]
matches[,IsOver:=0]
matches[TotalGoals>2,IsOver:=1]
matches[,Is1 := HomeGoals > AwayGoals]
matches[,Is2 := HomeGoals < AwayGoals]
matches[,IsX := HomeGoals == AwayGoals]
matches[,results := Is1*1 + Is2*2 + IsX*3]
matches <- matches[complete.cases(matches)]

bmakers <- c("10Bet", "Betfair Exchange", "bet365", "Betclic", "1xBet")

odds <- odds %>% filter(bookmaker %in% bmakers)  %>% mutate()
odds <- as.data.table(odds)
odds_final=odds[,list(final_odd=odd[.N]),
                              by=list(matchId,oddtype,bookmaker)]
wide_final <- dcast(odds_final,
                            matchId ~ bookmaker + oddtype,
                            value.var="final_odd")

pca <- princomp(na.omit(wide_final[2:.N,2:53]))

summary(pca)

plot(pca$scores, col = matches$IsOver + 2)
plot(pca$scores, col = matches$results + 1)

a <- dist(wide_final[,2:ncol(wide_final)], method = "euclidean")


fit <- cmdscale(a)
summary(fit)
fit
plot(fit[,1],fit[,2],main='Location',xlab='', ylab='',col=1)


odds_10Bet <- odds[bookmaker == "10Bet"]
odds_10Bet_final=odds_10Bet[,list(final_odd=odd[.N]),
                by=list(matchId,oddtype,bookmaker)]
wide_10Bet_final <- dcast(odds_10Bet_final,
                    matchId ~ bookmaker + oddtype,
                    value.var="final_odd")

pca <- princomp(na.omit(wide_10Bet_final[2:.N,2:ncol(wide_10Bet_final)]))
plot(pca$scores, col = matches$IsOver + 2)
plot(pca$scores, col = matches$results + 1)


a <- dist(wide_10Bet_final[,2:ncol(wide_10Bet_final)], method = "manhattan")


fit <- cmdscale(a)
summary(fit)
fit
plot(fit[,1],fit[,2],main='Location',xlab='', ylab='')

