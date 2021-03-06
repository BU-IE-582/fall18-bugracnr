require(data.table)
require(anytime)

#save paths
matches_file_path = "HW1_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_file_path = "HW1_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

#load data
matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)


#keep only unique matches
matches=unique(matches)

#keep year data
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

#Keep only complete data
matches=matches[complete.cases(matches)]

#delete unnecessary
matches[,c("HomeGoals","AwayGoals","TotalGoals") := NULL]


#keep only over & under matches with 2.5 handicap
odds_ov_un=odds[betType=='ou' & totalhandicap=='2.5']



#order data in ascending date & find initial and final matches
odds_ov_un=odds_ov_un[order(matchId, oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ov_un[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_ov_un_final=odds_ov_un[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

							#introduce cutpoints to be analyzed
cutpoints=seq(0,1,0.05)

#Probabilities for initial data
wide_ov_un_initial <- dcast(odds_ov_un_initial,
                              matchId + bookmaker ~oddtype,
                              value.var="start_odd")


initial_merged_data <- merge(matches[,c("matchId", "IsOver","Year")],wide_ov_un_initial,by='matchId')
initial_merged_data[,probOver:=1/over]
initial_merged_data[,probUnder:=1/under]
initial_merged_data[,totalProb:=probOver+probUnder]
initial_merged_data[,probOver:=probOver/totalProb]
initial_merged_data[,probUnder:=probUnder/totalProb]
initial_merged_data=initial_merged_data[complete.cases(initial_merged_data)]
initial_merged_data[,totalProb:=NULL]
initial_merged_data[,odd_cut_over:=cut(probOver,cutpoints)]


#Probabilites for the final data
wide_ov_un_final <- dcast(odds_ov_un_final,
                     matchId + bookmaker ~oddtype,
                     value.var="final_odd")

  final_merged_data <- merge(matches[,c("matchId", "IsOver","Year")],wide_ov_un_final,by='matchId')
  final_merged_data[,probOver:=1/over]
  final_merged_data[,probUnder:=1/under]
  final_merged_data[,totalProb:=probOver+probUnder]
  final_merged_data[,probOver:=probOver/totalProb]
  final_merged_data[,probUnder:=probUnder/totalProb]
  final_merged_data=final_merged_data[complete.cases(final_merged_data)]
  final_merged_data[,totalProb:=NULL]
  final_merged_data[,odd_cut_over:=cut(probOver,cutpoints)]


#pinnacle is the first bookmaker that I choose, first initial odds analysis
pinnacle_initial <- initial_merged_data[bookmaker == "Pinnacle"]
pinnacle_final <- final_merged_data[bookmaker == "Pinnacle"]


pinnacle_initial_summary=pinnacle_initial[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(odd_cut_over)]


pinnacle_final_summary=pinnacle_final[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]
par(bg="gray")

#Task 1.a
plot(pinnacle_initial_summary[,list(empirical_over,probabilistic_over)],cex=1, col = "blue", pch = 2, xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Probabilities")
points(pinnacle_final_summary[,list(empirical_over,probabilistic_over)],cex=1, col = "red", pch = 5)
abline(0,1,col='green')


#analysis over years
pinnacle_final_yearly_analysis=pinnacle_final[,list(empirical_over=mean(IsOver),
                                                    probabilistic_over=mean(probOver),.N),
                                              by=list(Year,odd_cut_over)]

pinnacle_final_yearly_analysis=pinnacle_final_yearly_analysis[order(Year)]


pinnacle_initial_yearly_analysis=pinnacle_initial[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(Year,odd_cut_over)]

pinnacle_initial_yearly_analysis=pinnacle_initial_yearly_analysis[order(Year)]

#2 plot functions are introduced
plotYears <- function(initial_data, final_data, minprob, maxprob)
{
  probCut <- paste("(", minprob, ",", maxprob, "]", sep = "")
  minY <- min(initial_data[odd_cut_over == probCut,list(probabilistic_over)],final_data[odd_cut_over == probCut,list(probabilistic_over)])
  maxY <- max(initial_data[odd_cut_over == probCut,list(probabilistic_over)],final_data[odd_cut_over == probCut,list(probabilistic_over)])
  minX <- min(initial_data[odd_cut_over == probCut,list(empirical_over)],final_data[odd_cut_over == probCut,list(empirical_over)])
  maxX <- 0.1 + max(initial_data[odd_cut_over == probCut,list(empirical_over)],final_data[odd_cut_over == probCut,list(empirical_over)])

  plot(initial_data[odd_cut_over == probCut,list(empirical_over,probabilistic_over)], ylim = c(minY, maxY), xlim = c(minX, maxX),cex=2, pch = 2, 
       xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Values by Years", col= initial_data[odd_cut_over == probCut]$Year - 2010)
  points(final_data[odd_cut_over == probCut,list(empirical_over,probabilistic_over)],cex=2, pch = 5, col= final_data[odd_cut_over == probCut]$Year - 2010)
  abline(0,1,col='red')
  abline(v = minprob)
  abline(v = maxprob)
  legend("topright",cex = 0.55, legend = initial_data[odd_cut_over == probCut]$Year, 
       fill = initial_data[odd_cut_over == probCut]$Year - 2010)
}


plotSeries <- function(initial_data, final_data, minprob, maxprob)
{
  probCut <- paste("(", minprob, ",", maxprob, "]", sep = "")
  minVal <- min(initial_data[odd_cut_over == probCut,list(empirical_over)],
                final_data[odd_cut_over == probCut,list(empirical_over)])
  maxVal <- max(initial_data[odd_cut_over == probCut,list(empirical_over)],
                final_data[odd_cut_over == probCut,list(empirical_over)])
  plot(initial_data[odd_cut_over == probCut,list(Year,empirical_over)], ylab = "Empirical Over", main = "Change in Probabilities Over Time",
       ylim = c(minVal,maxVal), cex=2, pch = 1, col= initial_data[odd_cut_over == probCut]$Year - 2010)
  points(final_data[odd_cut_over == probCut,list(Year,empirical_over)],
         cex=2, pch = 2, col= initial_data[odd_cut_over == probCut]$Year - 2010)
  lines(initial_data[odd_cut_over == probCut,list(Year,empirical_over)], col = "blue")
  lines(final_data[odd_cut_over == probCut,list(Year,empirical_over)], col = "red")
  abline(h = minprob, col = "black")
  abline(h = maxprob, col = "black")
}

#results
plotYears(pinnacle_initial_yearly_analysis,pinnacle_final_yearly_analysis,0.5,0.55)
plotSeries(pinnacle_initial_yearly_analysis,pinnacle_final_yearly_analysis,0.5,0.55)


#####Betsafe Part 1

betsafe_initial <- initial_merged_data[bookmaker == "Betsafe"]
betsafe_final <- final_merged_data[bookmaker == "Betsafe"]


betsafe_initial_summary=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                probabilistic_over=mean(probOver),.N),
                                          by=list(odd_cut_over)]


betsafe_final_summary=betsafe_final[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]


plot(betsafe_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2,xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Probabilities")
points(betsafe_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')

####Task 1-b


betsafe_final_yearly_analysis=betsafe_final[,list(empirical_over=mean(IsOver),
                                                    probabilistic_over=mean(probOver),.N),
                                              by=list(Year,odd_cut_over)]

betsafe_final_yearly_analysis=betsafe_final_yearly_analysis[order(Year)]


betsafe_initial_yearly_analysis=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                        probabilistic_over=mean(probOver),.N),
                                                  by=list(Year,odd_cut_over)]

betsafe_initial_yearly_analysis=betsafe_initial_yearly_analysis[order(Year)]


plotYears(betsafe_initial_yearly_analysis,betsafe_final_yearly_analysis,0.5,0.55)
plotSeries(betsafe_initial_yearly_analysis,betsafe_final_yearly_analysis,0.5,0.55)


######Sportingbet

sportingbet_initial <- initial_merged_data[bookmaker == "Sportingbet"]
sportingbet_final <- final_merged_data[bookmaker == "Sportingbet"]


sportingbet_initial_summary=sportingbet_initial[,list(empirical_over=mean(IsOver),
                                              probabilistic_over=mean(probOver),.N),
                                        by=list(odd_cut_over)]


sportingbet_final_summary=sportingbet_final[,list(empirical_over=mean(IsOver),
                                          probabilistic_over=mean(probOver),.N),
                                    by=list(odd_cut_over)]

par(bg="gray")
plot(sportingbet_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2, xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Probabilities")
points(sportingbet_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')

####Task 1-b

sportingbet_final_yearly_analysis=sportingbet_final[,list(empirical_over=mean(IsOver),
                                                  probabilistic_over=mean(probOver),.N),
                                            by=list(Year,odd_cut_over)]

sportingbet_final_yearly_analysis=sportingbet_final_yearly_analysis[order(Year)]


sportingbet_initial_yearly_analysis=sportingbet_initial[,list(empirical_over=mean(IsOver),
                                                      probabilistic_over=mean(probOver),.N),
                                                by=list(Year,odd_cut_over)]

sportingbet_initial_yearly_analysis=sportingbet_initial_yearly_analysis[order(Year)]

par(bg="gray")
plotYears(sportingbet_initial_yearly_analysis,sportingbet_final_yearly_analysis,0.5,0.55)
plotSeries(sportingbet_initial_yearly_analysis,sportingbet_final_yearly_analysis,0.5,0.55)


#####Tipico

tipico_initial <- initial_merged_data[bookmaker == "Tipico"]
tipico_final <- final_merged_data[bookmaker == "Tipico"]


tipico_initial_summary=tipico_initial[,list(empirical_over=mean(IsOver),
                                                      probabilistic_over=mean(probOver),.N),
                                                by=list(odd_cut_over)]


tipico_final_summary=tipico_final[,list(empirical_over=mean(IsOver),
                                                  probabilistic_over=mean(probOver),.N),
                                            by=list(odd_cut_over)]

par(bg="gray")
plot(tipico_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2, xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Probabilities")
points(tipico_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')

####Task 1-b


tipico_final_yearly_analysis=tipico_final[,list(empirical_over=mean(IsOver),
                                                          probabilistic_over=mean(probOver),.N),
                                                    by=list(Year,odd_cut_over)]

tipico_final_yearly_analysis=tipico_final_yearly_analysis[order(Year)]


tipico_initial_yearly_analysis=tipico_initial[,list(empirical_over=mean(IsOver),
                                                              probabilistic_over=mean(probOver),.N),
                                                        by=list(Year,odd_cut_over)]

tipico_initial_yearly_analysis=tipico_initial_yearly_analysis[order(Year)]

par(bg="gray")
plotYears(tipico_initial_yearly_analysis,tipico_final_yearly_analysis,0.5,0.55)
plotSeries(tipico_initial_yearly_analysis,tipico_final_yearly_analysis,0.5,0.55)

#####Interwetten



interwetten_initial <- initial_merged_data[bookmaker == "Interwetten"]
interwetten_final <- final_merged_data[bookmaker == "Interwetten"]


interwetten_initial_summary=interwetten_initial[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]


interwetten_final_summary=interwetten_final[,list(empirical_over=mean(IsOver),
                                        probabilistic_over=mean(probOver),.N),
                                  by=list(odd_cut_over)]

par(bg="gray")
plot(interwetten_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2, xlab = "Empirical Over", ylab = "Probabilistic Over", main = "Comparison of Initial and Final Probabilities")
points(interwetten_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')

####Task 1-b


interwetten_final_yearly_analysis=interwetten_final[,list(empirical_over=mean(IsOver),
                                                probabilistic_over=mean(probOver),.N),
                                          by=list(Year,odd_cut_over)]

interwetten_final_yearly_analysis=interwetten_final_yearly_analysis[order(Year)]


interwetten_initial_yearly_analysis=interwetten_initial[,list(empirical_over=mean(IsOver),
                                                    probabilistic_over=mean(probOver),.N),
                                              by=list(Year,odd_cut_over)]

interwetten_initial_yearly_analysis=interwetten_initial_yearly_analysis[order(Year)]

par(bg="gray")
plotYears(interwetten_initial_yearly_analysis,interwetten_final_yearly_analysis,0.5,0.55)
plotSeries(interwetten_initial_yearly_analysis,interwetten_final_yearly_analysis,0.5,0.55)


############### Task 2

#filter required data
odds_1x2 <- odds[odds$betType == "1x2",]
odds_1x2
odds_1x2[,totalhandicap:=NULL]
odds_1x2 <- odds_1x2[complete.cases(odds_1x2)]

odds_1x2=odds_1x2[order(matchId, oddtype,bookmaker,date)]

odds_1x2_initial=odds_1x2[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_1x2_final=odds_1x2[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]
odds1 <- odds_1x2[oddtype == "odd1"]


#### effect of time on number of changes #####
number_of_changes <- odds1[,list(changeNum = length(odd)), by = list(matchId, oddtype, bookmaker)]
number_of_changes <- merge(number_of_changes, matches[,c("matchId", "Year")], by = "matchId")
number_of_changes[complete.cases(number_of_changes)]


#distribution
hist(number_of_changes$changeNum)

large_changes <- number_of_changes[changeNum > 100]
hist(large_changes$changeNum, xlab= "Number of Changes", main = "Histogram of Large Number of Changes")

#change in mean number of changes
mean_changes <- number_of_changes[,list(mean=mean(changeNum)), by=list(Year)]
mean_changes <- mean_changes[order(Year)]
plot(mean_changes, type = "o", col = "blue", ylab = "Mean Changes", main = "Average Number of Changes per Match over Time")





###wide all data
#generate probabilities for 1x2 odds
wide_initial_1x2 <- dcast(odds_1x2_initial,
                           matchId + bookmaker ~oddtype,
                           value.var="start_odd")

wide_initial_1x2[,prob1 := 1/odd1]
wide_initial_1x2[,prob2 := 1/odd2]
wide_initial_1x2[,probx := 1/oddX]
wide_initial_1x2[,totalprob := prob1 + prob2 + probx]
wide_initial_1x2
wide_initial_1x2[,prob1 := prob1/totalprob]
wide_initial_1x2[,prob2 := prob2/totalprob]
wide_initial_1x2[,probx := probx/totalprob]
wide_initial_1x2[,totalprob := NULL]
wide_initial_1x2[complete.cases(wide_initial_1x2)]
wide_initial_1x2[,prob1_cut:=cut(prob1,cutpoints)]
wide_initial_1x2[,prob2_cut:=cut(prob2,cutpoints)]
wide_initial_1x2[,probx_cut:=cut(probx,cutpoints)]
wide_initial_1x2 <- merge(wide_initial_1x2,matches[,c("matchId", "Is1","Is2","IsX")],by='matchId')

#same thing for final data
wide_final_1x2 <- dcast(odds_1x2_final,
      matchId + bookmaker ~oddtype,
      value.var="final_odd")

wide_final_1x2[,prob1 := 1/odd1]
wide_final_1x2[,prob2 := 1/odd2]
wide_final_1x2[,probx := 1/oddX]
wide_final_1x2[,totalprob := prob1 + prob2 + probx]
wide_final_1x2
wide_final_1x2[,prob1 := prob1/totalprob]
wide_final_1x2[,prob2 := prob2/totalprob]
wide_final_1x2[,probx := probx/totalprob]
wide_final_1x2[,totalprob := NULL]
wide_final_1x2[complete.cases(wide_final_1x2)]
wide_final_1x2[,prob1_cut:=cut(prob1,cutpoints)]
wide_final_1x2[,prob2_cut:=cut(prob2,cutpoints)]
wide_final_1x2[,probx_cut:=cut(probx,cutpoints)]
wide_final_1x2 <- merge(wide_final_1x2,matches[,c("matchId", "Is1","Is2","IsX")],by='matchId')


#histograms are saved as seperate objects
initial_prob1_hist <- hist(wide_initial_1x2$prob1)
initial_prob2_hist <- hist(wide_initial_1x2$prob2)
initial_probx_hist <- hist(wide_initial_1x2$probx)

final_prob1_hist <- hist(wide_final_1x2$prob1)
final_prob2_hist <- hist(wide_final_1x2$prob2)
final_probx_hist <- hist(wide_final_1x2$probx)



#plot the initial and final histograms in the same plot for comparison
plot(initial_prob1_hist, col = rgb(1,0,0,1/4), xlim = c(0,1), xlab =  "Probability", main = "Initial and Final 1 Distributions")
plot(final_prob1_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_prob1_hist$mids, initial_prob1_hist$counts,  col = "red", type = "o")
lines(final_prob1_hist$mids, final_prob1_hist$counts, col = "blue", type = "o")


plot(initial_prob2_hist, col = rgb(1,0,0,1/4), xlim = c(0,1), xlab =  "Probability", main = "Initial and Final 2 Distributions")
plot(final_prob2_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_prob2_hist$mids, initial_prob2_hist$counts,  col = "red", type = "o")
lines(final_prob2_hist$mids, final_prob2_hist$counts, col = "blue", type = "o")

plot(initial_probx_hist, col = rgb(1,0,0,1/4), xlim = c(0,1), xlab =  "Probability", main = "Initial and Final X Distributions")
plot(final_probx_hist, col = rgb(0,0,1,1/4), xlim = c(0,1), add = TRUE)
lines(initial_probx_hist$mids, initial_probx_hist$counts,  col = "red", type = "o")
lines(final_probx_hist$mids, final_probx_hist$counts, col = "blue", type = "o")





