library(dplyr)
require(data.table)
require(anytime)

rm(list=ls())
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

rm(odds)
gc()

#order data in ascending date & find initial and final matches
odds_ov_un=odds_ov_un[order(matchId, oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ov_un[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_ov_un_final=odds_ov_un[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]




#pinnacle is the first bookmaker that I choose, first initial odds analysis
pinnacle_over_under=odds_ov_un_initial[bookmaker=='Pinnacle']

pinnacle_wide_initial=dcast(pinnacle_over_under,
                    matchId~oddtype,
                    value.var='start_odd')

pinnacle_initial=merge(matches[,c("matchId", "IsOver","Year")],pinnacle_wide_initial,by='matchId')


pinnacle_initial[,probOver:=1/over]
pinnacle_initial[,probUnder:=1/under]

pinnacle_initial[,totalProb:=probOver+probUnder]

pinnacle_initial[,probOver:=probOver/totalProb]
pinnacle_initial[,probUnder:=probUnder/totalProb]



pinnacle_initial=pinnacle_initial[complete.cases(pinnacle_initial)]
pinnacle_initial[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
pinnacle_initial[,odd_cut_over:=cut(probOver,cutpoints)]

#initial aggregate analysis
pinnacle_initial_summary=pinnacle_initial[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(odd_cut_over)]




#final aggregate analysis
#pinnacle final odds analysis
pinnacle_over_under=odds_ov_un_final[bookmaker=='Pinnacle']

pinnacle_wide_final=dcast(pinnacle_over_under,
                          matchId~oddtype,
                          value.var='final_odd')

pinnacle_final=merge(matches[,c("matchId", "IsOver","Year")],pinnacle_wide_final,by='matchId')


pinnacle_final[,probOver:=1/over]
pinnacle_final[,probUnder:=1/under]

pinnacle_final[,totalProb:=probOver+probUnder]

pinnacle_final[,probOver:=probOver/totalProb]
pinnacle_final[,probUnder:=probUnder/totalProb]



pinnacle_final=pinnacle_final[complete.cases(pinnacle_final)]
pinnacle_final[,totalProb:=NULL]

pinnacle_final[,odd_cut_over:=cut(probOver,cutpoints)]

pinnacle_final_summary=pinnacle_final[,list(empirical_over=mean(IsOver),
                                            probabilistic_over=mean(probOver),.N),
                                      by=list(odd_cut_over)]

#total aggreagate analysis
plot(pinnacle_initial_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "blue", pch = 2)
points(pinnacle_final_summary[,list(empirical_over,probabilistic_over)],cex=2, col = "purple", pch = 5)
abline(0,1,col='red')

pinnacle_initial_summary
pinnacle_final_summary

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
pinnacle_initial_yearly_analysis[odd_cut_over == "(0.55,0.6]"]
pinnacle_final_yearly_analysis[odd_cut_over == "(0.55,0.6]"]
plot(pinnacle_initial_yearly_analysis[odd_cut_over == "(0.55,0.6]",list(empirical_over,probabilistic_over)],cex=2, pch = 2, col= pinnacle_initial_yearly_analysis[odd_cut_over == "(0.55,0.6]"]$Year - 2010)
points(pinnacle_final_yearly_analysis[odd_cut_over == "(0.55,0.6]",list(empirical_over,probabilistic_over)],cex=2, pch = 5, col= pinnacle_final_yearly_analysis[odd_cut_over == "(0.55,0.6]"]$Year - 2010)
abline(0,1,col='red')
abline(v = 0.55)
abline(v = 0.60)
legend("topright",cex = 0.75, legend = pinnacle_initial_yearly_analysis[odd_cut_over == "(0.55,0.6]"]$Year, 
       fill = pinnacle_initial_yearly_analysis[odd_cut_over == "(0.55,0.6]"]$Year - 2010)


#bookmaker 2: Betsafe
betsafe_over_under=odds_ov_un_initial[bookmaker=='Betsafe']

betsafe_wide_initial=dcast(betsafe_over_under,
                            matchId~oddtype,
                            value.var='start_odd')

betsafe_initial=merge(matches[,c("matchId", "IsOver","Year")],betsafe_wide_initial,by='matchId')


betsafe_initial[,probOver:=1/over]
betsafe_initial[,probUnder:=1/under]

betsafe_initial[,totalProb:=probOver+probUnder]

betsafe_initial[,probOver:=probOver/totalProb]
betsafe_initial[,probUnder:=probUnder/totalProb]



betsafe_initial=betsafe_initial[complete.cases(betsafe_initial)]
betsafe_initial[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
betsafe_initial[,odd_cut_over:=cut(probOver,cutpoints)]

#aggregate analysis
betsafe_initial_summary=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                probabilistic_over=mean(probOver),.N),
                                          by=list(odd_cut_over)]


plot(betsafe_initial_summary[,list(empirical_over,probabilistic_over)],cex=4)
abline(0,1,col='red')

#analysis by years
betsafe_initial_yearly_analysis=betsafe_initial[,list(empirical_over=mean(IsOver),
                                                        probabilistic_over=mean(probOver),.N),
                                                  by=list(Year,odd_cut_over)]

betsafe_initial_yearly_analysis=betsafe_initial_yearly_analysis[order(Year)]

plot(betsafe_initial_yearly_analysis[,list(empirical_over,probabilistic_over)],cex=1, col= betsafe_initial_yearly_analysis$Year - 2009)
abline(0,1,col='red')

#betsafe final odds analysis
betsafe_over_under=odds_ov_un_final[bookmaker=='Betsafe']

betsafe_wide_final=dcast(betsafe_over_under,
                          matchId~oddtype,
                          value.var='final_odd')

betsafe_final=merge(matches[,c("matchId", "IsOver","Year")],betsafe_wide_final,by='matchId')


betsafe_final[,probOver:=1/over]
betsafe_final[,probUnder:=1/under]

betsafe_final[,totalProb:=probOver+probUnder]

betsafe_final[,probOver:=probOver/totalProb]
betsafe_final[,probUnder:=probUnder/totalProb]



betsafe_final=betsafe_final[complete.cases(betsafe_final)]
betsafe_final[,totalProb:=NULL]

betsafe_final[,odd_cut_over:=cut(probOver,cutpoints)]

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

