require(data.table)
require(anytime)
require(dplyr)

setwd("C:/IE_582_Rep/fall18-bugracnr/HW2")

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
matches <- unique(matches)
matches <- matches[complete.cases(matches)]

bmakers <- c("Betsafe", "12BET", "bet365", "Betclic", "Pinnacle")
bettypes <- unique(odds$betType)
bettypes <- bettypes[c(-2, -6)]

odds_ou <- odds%>% filter(bookmaker %in% bmakers, betType == "ou", totalhandicap == "2.5") %>% mutate() 
odds <- odds %>% filter(bookmaker %in% bmakers, betType %in%bettypes)  %>% mutate()
odds <- rbind(odds,odds_ou)
odds <- as.data.table(odds)
odds <- odds[order(matchId, oddtype,bookmaker,date)]
odds <- unique(odds)

odds_final=odds[,list(final_odd=odd[.N]),
                              by=list(matchId,oddtype,bookmaker)]
wide_final <- dcast(odds_final,
                            matchId ~ bookmaker + oddtype,
                            value.var="final_odd")

wide_final <- wide_final[complete.cases(wide_final)]
wide_final <- unique(wide_final)
wide_final <- wide_final[matchId %in% matches$matchId]

match_colors <- merge(wide_final[,"matchId"], matches[,c("matchId", "IsOver", "results")], by = "matchId")



pca <- princomp(wide_final[,2:ncol(wide_final)])

biplot(pca)

summary(pca)
plot(pca)

par(mfrow = c(2,1))
plot(pca$scores, col = match_colors$IsOver + 2)
plot(pca$scores, col = match_colors$results + 1)


manhattan_distances <- dist(na.omit(wide_final[,2:ncol(wide_final)]), method = "manhattan")
euclidean_distances <- dist(na.omit(wide_final[,2:ncol(wide_final)]), method = "euclidean")

fit_manhattan <- cmdscale(manhattan_distances)
plot(fit_manhattan[,1],fit_manhattan[,2],main='Location',xlab='', ylab='',col= match_colors$IsOver + 2)

fit_euclidean <- cmdscale(euclidean_distances)
plot(fit_euclidean[,1],fit_euclidean[,2],main='Location',xlab='', ylab='',col= match_colors$IsOver + 2)



