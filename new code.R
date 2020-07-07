setwd('~/Documents/GitHub/Rajya-Sabha/')
library(tidyr)
library(dplyr)
allmembers<- read.csv("all_rs_members_scraped.csv")
allmembers <- separate(allmembers, vacation_date_reason, c("vacate_date", "vacate_reason"), "\n", TRUE, TRUE) #to separate the vacate reason into date & reason 
allmembers <- separate(allmembers,term_from_to, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column

library(lubridate)
library(data.table)
allmembers = data.table(allmembers)
allmembers$vacation_date =dmy(allmembers$vacation_date_reason)
allmembers$vacatereason = gsub("[^[:alpha:]]", "", allmembers$vacation_date_reason)
print(allmembers[is.na(vacatereason),])
print(unique(allmembers$vacatereason))

allmembers$term_start = dmy(allmembers$term_start)
allmembers$term_end = dmy(allmembers$term_end)

Members5 <- allmembers[year(term_start)>=1950 & year(term_start)<1960,]
unique_parties_rs5 = unique(allmembers5$party) #unique parties in the 1950's

Members6 <- allmembers[year(term_start)>=1960 & year(term_start)<1970,]
unique_parties_rs6 = unique(allmembers6$party) #unique parties in the 1960's

Members7 <- allmembers[year(term_start)>=1970 & year(term_start)<1980,]
unique_parties_rs7 = unique(allmembers7$party) #unique parties in the 1970's

Members8 <- allmembers[year(term_start)>=1980 & year(term_start)<1990,]
unique_parties_rs8 = unique(allmembers8$party) #unique parties in the 1980's

Members9 <- allmembers[year(term_start)>=1990 & year(term_start)<2000,]
unique_parties_rs9 = unique(allmembers9$party) #unique parties in the 1990's

Members0 <- allmembers[year(term_start)>=2000 & year(term_start)<2010,]
unique_parties_rs0 = unique(allmembers0$party) #unique parties in the 2000's

Members1 <- allmembers[year(term_start)>=2010 & year(term_start)<2020,]
unique_parties_rs1 = unique(allmembers1$party) #unique parties in the 2010's

NormalizedParties<- read.csv("all_normalized_party_names.csv")
