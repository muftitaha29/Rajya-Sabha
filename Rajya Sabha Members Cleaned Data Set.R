setwd('~/Documents/GitHub/Rajya-Sabha/')
library(tidyr)
library(dplyr)
Members <- read.csv("rs_member_terms.csv") # defining the data frame
Members <- separate(Members, vacate_reason, c("vacate_date", "vacate_reason"), "\n", TRUE, TRUE) #to separate the vacate reason into date & reason 
Members <- separate(Members,term, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column
Members <- Members %>% rowwise %>%  mutate(vacate_reason = ifelse(is.na(vacate_reason), vacate_date, vacate_reason ) ) #running the operation row wise

filter(Members, is.na(Members$vacate_date))
filter(Members, is.na(Members$term_start))
filter(Members, is.na(Members$term_end))
uniquestates<- unique(Members$state)
print(unique(Members$vacate_reason))

library(data.table)

for(i in 1:nrow(Members)){
   if(Members$vacate_date[i]=='Retirement' | Members$vacate_date[i]=='Reorganisation of the State' | Members$vacate_date[i]==''){
         Members$vacate_date[i] = Members$term_end[i]
       }
    
     }

library(lubridate)
Members = data.table(Members)
Members$term_start = dmy(Members$term_start)
Members$term_end = dmy(Members$term_end)


Members8 <- Members[year(term_start)>=1980 & year(term_start)<1990,]
unique_parties_rs8 = unique(Members8$party) #unique parties in the 1980's

Members7 <- Members[year(term_start)>=1970 & year(term_start)<1980,]
unique_parties_rs7 = unique(Members7$party) #unique parties in the 1970's

Members5 <- Members[year(term_start)>=1950 & year(term_start)<1960,]
unique_parties_rs5 = unique(Members$party) #unique parties in the 1950's

Members6 <- Members[year(term_start)>=1960 & year(term_start)<1970,]
unique_parties_rs6 = unique(Members6$party) #unique parties in the 1960's




NormalizedParties<- read.csv("all_normalized_party_names.csv")

NormalizedParties = NormalizedParties[NormalizedParties$Election_Type=='GE',]
NormalizedParties8 = NormalizedParties[NormalizedParties$Year>=1980 & NormalizedParties$Year < 1990,]
NormalizedParties8 = unique(NormalizedParties8$Expanded.Party.Name)
