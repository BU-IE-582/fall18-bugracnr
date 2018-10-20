require(data.table)
require(anytime)
require(dplyr)
library(rgl)
setwd("C:/IE_582_Rep/fall18-bugracnr/HW2")
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


bmakers <- c("10Bet", "bwin", "Expekt", "Pinnacle", "1xBet")

odds <- odds %>% filter(bookmaker %in% bmakers)  %>% mutate()
str(odds)
odds <- as.data.table(odds)

odds_ou <- odds[betType == "ou" & totalhandicap == "2.5"]

class(a)
unique(odds_rest$betType)
unique(odds[odds$betType == "ha"]$totalhandicap)

odds_rest <- odds[odds$betType != "ou",]
odds_rest <- odds_rest[odds_rest$betType != "ah",]
unique(odds_rest$totalhandicap)
class(odds_rest)
class(matches)
odds_ou[order(matchId,oddtype,bookmaker,date)]
odds_rest[order(matchId,oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ou[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]
odds_rest_initial = odds_rest[,list(start_odd=odd[1]),
                            by=list(matchId,oddtype,bookmaker)]



odds_ov_un_initial <- odds_ov_un_initial %>% filter(bookmaker %in% bmakers)  %>% mutate()
odds_rest_initial <- odds_rest_initial %>% filter(bookmaker %in% bmakers)  %>% mutate()
unique(odds_ov_un_initial$bookmaker)
unique(odds_rest_initial$bookmaker)

unique(odds$bookmaker)
matches <- matches[complete.cases(matches)]
wide_ov_un_initial <- dcast(odds_ov_un_initial,
                            matchId ~ bookmaker + oddtype,
                            value.var="start_odd")
wide_rest_initial <- dcast(odds_rest_initial,
                            matchId ~ bookmaker + oddtype,
                            value.var="start_odd")

odds_ov_un_initial[matchId =="004f4ING"]

wide_all <- merge(wide_ov_un_initial,wide_rest_initial, by = "matchId")
wide_all <- merge(matches[,c("matchId","IsOver")],wide_all, by.x = "matchId",by.y = "matchId")




pca <- princomp(na.omit(wide_all[2:.N,2:53]), cor = TRUE)




odds_10Bet <- odds[bookmaker == "Pinnacle"]


odds_ou_10Bet <- odds_10Bet[betType == "ou" & totalhandicap == "2.5"]


odds_rest_10Bet <- odds_10Bet[odds_10Bet$betType != "ou",]
odds_rest_10Bet <- odds_rest_10Bet[odds_rest_10Bet$betType != "ah",]

odds_ou_10Bet[order(matchId,oddtype,bookmaker,date)]
odds_rest_10Bet[order(matchId,oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ou_10Bet[,list(start_odd=odd[1]),
                           by=list(matchId,oddtype,bookmaker)]
odds_rest_initial = odds_rest_10Bet[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]




matches <- matches[complete.cases(matches)]
wide_ov_un_initial <- dcast(odds_ov_un_initial,
                            matchId ~ bookmaker + oddtype,
                            value.var="start_odd")
wide_rest_initial <- dcast(odds_rest_initial,
                           matchId ~ bookmaker + oddtype,
                           value.var="start_odd")


wide_all <- merge(wide_ov_un_initial,wide_rest_initial, by = "matchId")
wide_all <- merge(matches[,c("matchId","IsOver")],wide_all, by.x = "matchId",by.y = "matchId")

pca <- princomp(na.omit(wide_all[2:.N,2:7]), cor = TRUE)


biplot(pca)

summary(pca)
str(pca)
plot(pca)
plot(pca$scores,ylim=c(-4,4),xlim=c(-4,4), col = wide_all$IsOver+2)

head(pca)


plot3d(pca$scores[,1:3], col = wide_all$IsOver + 2)

odds_10Bet <- odds[bookmaker == "Expekt"]


odds_ou_10Bet <- odds_10Bet[betType == "ou" & totalhandicap == "2.5"]


odds_rest_10Bet <- odds_10Bet[odds_10Bet$betType != "ou",]
odds_rest_10Bet <- odds_rest_10Bet[odds_rest_10Bet$betType != "ah",]

odds_ou_10Bet[order(matchId,oddtype,bookmaker,date)]
odds_rest_10Bet[order(matchId,oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ou_10Bet[,list(start_odd=odd[1]),
                                 by=list(matchId,oddtype,bookmaker)]
odds_rest_initial = odds_rest_10Bet[,list(start_odd=odd[1]),
                                    by=list(matchId,oddtype,bookmaker)]




matches <- matches[complete.cases(matches)]
wide_ov_un_initial <- dcast(odds_ov_un_initial,
                            matchId ~ bookmaker + oddtype,
                            value.var="start_odd")
wide_rest_initial <- dcast(odds_rest_initial,
                           matchId ~ bookmaker + oddtype,
                           value.var="start_odd")


wide_all <- merge(wide_ov_un_initial,wide_rest_initial, by = "matchId")
wide_all <- merge(matches[,c("matchId","IsOver")],wide_all, by.x = "matchId",by.y = "matchId")

pca <- princomp(na.omit(wide_all[2:.N,2:7]), cor = TRUE)


biplot(pca)

summary(pca)
str(pca)
plot(pca)
plot(pca$scores,ylim=c(-4,4),xlim=c(-4,4), col = wide_all$IsOver+2)

head(pca)


odds_10Bet <- odds[bookmaker == "bwin"]


odds_ou_10Bet <- odds_10Bet[betType == "ou" & totalhandicap == "2.5"]


odds_rest_10Bet <- odds_10Bet[odds_10Bet$betType != "ou",]
odds_rest_10Bet <- odds_rest_10Bet[odds_rest_10Bet$betType != "ah",]

odds_ou_10Bet[order(matchId,oddtype,bookmaker,date)]
odds_rest_10Bet[order(matchId,oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ou_10Bet[,list(start_odd=odd[1]),
                                 by=list(matchId,oddtype,bookmaker)]
odds_rest_initial = odds_rest_10Bet[,list(start_odd=odd[1]),
                                    by=list(matchId,oddtype,bookmaker)]




matches <- matches[complete.cases(matches)]
wide_ov_un_initial <- dcast(odds_ov_un_initial,
                            matchId ~ bookmaker + oddtype,
                            value.var="start_odd")
wide_rest_initial <- dcast(odds_rest_initial,
                           matchId ~ bookmaker + oddtype,
                           value.var="start_odd")


wide_all <- merge(wide_ov_un_initial,wide_rest_initial, by = "matchId")
wide_all <- merge(matches[,c("matchId","IsOver")],wide_all, by.x = "matchId",by.y = "matchId")

pca <- princomp(na.omit(wide_all[2:.N,2:7]), cor = TRUE)


biplot(pca)

summary(pca)
str(pca)
plot(pca)
plot(pca$scores,ylim=c(-4,4),xlim=c(-4,4), col = wide_all$IsOver+2)

head(pca)



distmat <- dist(wide_all, method = "euclidean")
head(distmat)
mds=cmdscale(distmat)
plot(mds[,1],mds[,2],main='Location',xlab='', ylab='',col=0)
text(mds[,1],mds[,2],names(distmat),cex = .75,pos=4)
