require(data.table)
require(anytime)

rm(list=ls())
gc()

#save paths
matches_file_path = "HW2_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_file_path = "HW2_Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

#load data
matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)

unique(odds$bookmaker)

odds_subset <- odds[odds$bookmaker == c("10Bet", "12Bet", "Betway", "Pinnacle", "mybet"),]
odds_subset[, time := anytime(date)]
odds_subset[, Year := year(time)]
odds_subset <- odds_subset[order(matchId, oddtype, bookmaker, date)]
odds_subset <- odds_subset[,list(final_odd = odd[.N]), by=list(matchId,oddtype,bookmaker)]
odds_subset <- odds_subset[complete.cases(odds_subset)]

wide_odds <- dcast(odds_subset, matchId ~ bookmaker + oddtype, value.var = "final_odd")

wide_odds <- wide_odds[complete.cases(wide_odds)]


pca_data <- princomp(wide_odds)
