library(dplyr)
require(data.table)
require(anytime)

rm(list= ls())

gc()

#save paths of data files as strings
matches_file_path = "C:/IE582HW1/IE582Fall2018_project_files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_file_path = "C:/IE582HW1/IE582Fall2018_project_files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"


#load data
matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)


#keep only unique matches
matches=unique(matches)

#keep year data as we will use it later
matches[,match_time:=anytime(date)]
matches[,Year:=year(match_time)]
matches[,c("match_time","date","leagueId","type"):=NULL]


#find if a match is over or under
matches[,c("HomeGoals","AwayGoals"):=tstrsplit(score,':')]
#transform characters to numeric for scores
matches$HomeGoals=as.numeric(matches$HomeGoals)
matches[,AwayGoals:=as.numeric(AwayGoals)]

#calculate total goals
matches[,TotalGoals:=HomeGoals+AwayGoals]

# mark over under
matches[,IsOver:=0]
matches[TotalGoals>2,IsOver:=1]
#filter all NAs
matches=matches[complete.cases(matches)]

matches[,c("HomeGoals","AwayGoals","TotalGoals") := NULL]



#keep only over & under matches with 2.5 handicap
odds_ov_un=odds[betType=='ou' & totalhandicap=='2.5']



#order data in ascending date & find initial and final matches
odds_ov_un=odds_ov_un[order(matchId, oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ov_un[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_ov_un_final=odds_ov_un[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]


initialDataPrep <- function(bookmaker_name)
{
 
  wide_data <- dcast(odds_ov_un_initial[bookmaker==bookmaker_name],
                     matchId~oddtype,
                     value.var="start_odd")
  merged_data <- merge(matches[,c("matchId", "IsOver","Year")],wide_data,by='matchId')
  merged_data[,probOver:=1/over]
  merged_data[,probUnder:=1/under]
  
  merged_data[,totalProb:=probOver+probUnder]
  
  merged_data[,probOver:=probOver/totalProb]
  merged_data[,probUnder:=probUnder/totalProb]
  
  
  
  merged_data=merged_data[complete.cases(merged_data)]
  merged_data[,totalProb:=NULL]
  
  cutpoints=seq(0,1,0.05)
  merged_data[,odd_cut_over:=cut(probOver,cutpoints)]
}

finalDataPrep <- function(bookmaker_name)
{
  
  wide_data <- dcast(odds_ov_un_final[bookmaker==bookmaker_name],
                     matchId~oddtype,
                     value.var="final_odd")
  merged_data <- merge(matches[,c("matchId", "IsOver","Year")],wide_data,by='matchId')
  merged_data[,probOver:=1/over]
  merged_data[,probUnder:=1/under]
  
  merged_data[,totalProb:=probOver+probUnder]
  
  merged_data[,probOver:=probOver/totalProb]
  merged_data[,probUnder:=probUnder/totalProb]
  
  
  
  merged_data=merged_data[complete.cases(merged_data)]
  merged_data[,totalProb:=NULL]
  
  cutpoints=seq(0,1,0.05)
  merged_data[,odd_cut_over:=cut(probOver,cutpoints)]
}

#pinnacle is the first bookmaker that I choose, first initial odds analysis

pinnacle_initial <- initialDataPrep("Pinnacle")
pinnacle_final <- finalDataPrep("Pinnacle")


#initial aggregate analysis
pinnacle_initial_summary=pinnacle_initial[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(odd_cut_over)]


#final aggregate analysis

pinnacle_final_summary=pinnacle_final[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]

#total aggreagate analysis
plot(pinnacle_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2)
points(pinnacle_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')


#analysis by years
pinnacle_final_yearly_analysis=pinnacle_final[,list(empirical_over=mean(IsOver),
                                                    probabilistic_over=mean(probOver),.N),
                                              by=list(Year,odd_cut_over)]

pinnacle_final_yearly_analysis=pinnacle_final_yearly_analysis[order(Year)]

#analysis by years
pinnacle_initial_yearly_analysis=pinnacle_initial[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(Year,odd_cut_over)]

pinnacle_initial_yearly_analysis=pinnacle_initial_yearly_analysis[order(Year)]


plotYears <- function(initial_data, final_data, minprob, maxprob, probCut)
{
 minY <- min(initial_data[odd_cut_over == probCut,list(probabilistic_over)],final_data[odd_cut_over == probCut,list(probabilistic_over)])
   maxY <- max(initial_data[odd_cut_over == probCut,list(probabilistic_over)],final_data[odd_cut_over == probCut,list(probabilistic_over)])

   minX <- min(initial_data[odd_cut_over == probCut,list(empirical_over)],final_data[odd_cut_over == probCut,list(empirical_over)])

   maxX <- 0.1 + max(initial_data[odd_cut_over == probCut,list(empirical_over)],final_data[odd_cut_over == probCut,list(empirical_over)])
plot(initial_data[odd_cut_over == probCut,list(empirical_over,probabilistic_over)], ylim = c(minY, maxY), xlim = c(minX, maxX),cex=2, pch = 2, col= initial_data[odd_cut_over == probCut]$Year - 2010)
points(final_data[odd_cut_over == probCut,list(empirical_over,probabilistic_over)],cex=2, pch = 5, col= final_data[odd_cut_over == probCut]$Year - 2010)
abline(0,1,col='red')
abline(v = minprob)
abline(v = maxprob)
legend("topright",cex = 0.55, legend = initial_data[odd_cut_over == probCut]$Year, 
       fill = initial_data[odd_cut_over == probCut]$Year - 2010)
}

#time series data
### Special Plot Function
plotSeries <- function(initial_data, final_data, minprob, maxprob, probCut)
{
  minVal <- min(initial_data[odd_cut_over == probCut,list(empirical_over)],
                final_data[odd_cut_over == probCut,list(empirical_over)])
  maxVal <- max(initial_data[odd_cut_over == probCut,list(empirical_over)],
                final_data[odd_cut_over == probCut,list(empirical_over)])
  plot(initial_data[odd_cut_over == probCut,list(Year,empirical_over)], 
       ylim = c(minVal,maxVal), cex=2, pch = 3, col= initial_data[odd_cut_over == probCut]$Year - 2010)
  points(final_data[odd_cut_over == probCut,list(Year,empirical_over)],
         cex=2, pch = 4, col= initial_data[odd_cut_over == probCut]$Year - 2010)
  lines(initial_data[odd_cut_over == probCut,list(Year,empirical_over)], col = "blue")
  lines(final_data[odd_cut_over == probCut,list(Year,empirical_over)], col = "red")
  abline(h = minprob, col = "black")
  abline(h = maxprob, col = "black")
}


plotYears(pinnacle_initial_yearly_analysis,pinnacle_final_yearly_analysis, 0.45, 0.5, "(0.45,0.5]")


#aggregate analysis
betsafe_initial_summary=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                probabilistic_over=mean(probOver),.N),
                                          by=list(odd_cut_over)]


plot(betsafe_initial_summary[,list(empirical_over,probabilistic_over)],cex=4)
abline(0,1,col='red')

betsafe_initial <- initialDataPrep("Betsafe")
betsafe_final <- finalDataPrep("Betsafe")

#analysis by years
betsafe_initial_yearly_analysis=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                        probabilistic_over=mean(probOver),.N),
                                                  by=list(Year,odd_cut_over)]

betsafe_initial_yearly_analysis=betsafe_initial_yearly_analysis[order(Year)]

plot(betsafe_initial_yearly_analysis[,list(empirical_over,probabilistic_over)],cex=1, col= betsafe_initial_yearly_analysis$Year - 2009)
abline(0,1,col='red')

#betsafe final odds analysis

betsafe_final_summary=betsafe_final[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]


plot(betsafe_final_summary[,list(empirical_over,probabilistic_over)],cex=4)
abline(0,1,col='red')

#analysis by years
betsafe_final_yearly_analysis=betsafe_final[,list(empirical_over=mean(IsOver),
                                                    probabilistic_over=mean(probOver),.N),
                                              by=list(Year,odd_cut_over)]

betsafe_final_yearly_analysis=betsafe_final_yearly_analysis[order(Year)]

plot(betsafe_final_yearly_analysis[,list(empirical_over,probabilistic_over)],cex=4)
abline(0,1,col='red')

############### 1x2 Data will be worked on under this part
str(odds)
head(odds)
unique(odds$bookmaker)

odds_1x2 <- odds[odds$betType == "1x2",]
odds_1x2
odds_1x2[,totalhandicap:=NULL]
odds_1x2 <- odds_1x2[complete.cases(odds_1x2)]

odds_1x2=odds_1x2[order(matchId, oddtype,bookmaker,date)]

odds_1x2_initial=odds_1x2[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_1x2_final=odds_1x2[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

#merged_1x2 <- merge(odds_1x2_initial, odds_1x2_final, by = c("matchId", "oddtype", "bookmaker"))

###melted all data
melted_initial_1x2 <- dcast(odds_1x2_initial,
                           matchId + bookmaker ~oddtype,
                           value.var="start_odd")

melted_initial_1x2[,prob1 := 1/odd1]
melted_initial_1x2[,prob2 := 1/odd2]
melted_initial_1x2[,probx := 1/oddX]
melted_initial_1x2[,totalprob := prob1 + prob2 + probx]
melted_initial_1x2
melted_initial_1x2[,prob1 := prob1/totalprob]
melted_initial_1x2[,prob2 := prob2/totalprob]
melted_initial_1x2[,probx := probx/totalprob]
melted_initial_1x2[,totalprob := NULL]
melted_initial_1x2[complete.cases(melted_initial_1x2)]


summary(melted_initial_1x2)
summary(melted_final_1x2)

melted_final_1x2 <- dcast(odds_1x2_final,
      matchId + bookmaker ~oddtype,
      value.var="final_odd")

melted_final_1x2[,prob1 := 1/odd1]
melted_final_1x2[,prob2 := 1/odd2]
melted_final_1x2[,probx := 1/oddX]
melted_final_1x2[,totalprob := prob1 + prob2 + probx]
melted_final_1x2
melted_final_1x2[,prob1 := prob1/totalprob]
melted_final_1x2[,prob2 := prob2/totalprob]
melted_final_1x2[,probx := probx/totalprob]
melted_final_1x2[,totalprob := NULL]
melted_final_1x2[complete.cases(melted_final_1x2)]

initial_prob1_hist <- hist(melted_initial_1x2$prob1)
initial_prob2_hist <- hist(melted_initial_1x2$prob2)
initial_probx_hist <- hist(melted_initial_1x2$probx)
final_prob1_hist <- hist(melted_final_1x2$prob1)
final_prob2_hist <- hist(melted_final_1x2$prob2)
final_probx_hist <- hist(melted_final_1x2$probx)

plot(initial_prob1_hist, col = rgb(1,0,0,1/4), xlim = c(0,1))
plot(final_prob1_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_prob1_hist$mids, initial_prob1_hist$counts,  col = "red", type = "o")
lines(final_prob1_hist$mids, final_prob1_hist$counts, col = "blue", type = "o")


plot(initial_prob2_hist, col = rgb(1,0,0,1/4), xlim = c(0,1))
plot(final_prob2_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_prob2_hist$mids, initial_prob2_hist$counts,  col = "red", type = "o")
lines(final_prob2_hist$mids, final_prob2_hist$counts, col = "blue", type = "o")

plot(initial_probx_hist, col = rgb(1,0,0,1/4), xlim = c(0,1))
plot(final_probx_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_probx_hist$mids, initial_probx_hist$counts,  col = "red", type = "o")
lines(final_probx_hist$mids, final_probx_hist$counts, col = "blue", type = "o")
