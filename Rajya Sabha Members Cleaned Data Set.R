setwd("C:/Users/Mufti Taha Shah/Desktop/Ashoka/TCPD")
read.csv("rs_member_terms.csv")
library(tidyr)
Members <- read.csv("rs_member_terms.csv") # defining the data frame
Members <- separate(Members, vacate_reason, c("vacate_date", "vacate_reason"), "\n", TRUE, TRUE) #to separate the vacate reason into date & reason 
Members <- separate(Members,term, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column
Members <- mutate(Members,vacate_reason = ifelse(is.na(vacate_reason), vacate_date, vacate_reason ) ) #original code


Members <- Members %>% rowwise %>%  mutate(vacate_reason = ifelse(is.na(vacate_reason), vacate_date, vacate_reason ) ) #running the operation row wise

Members$vacate_date %>% as.Date() #changing type
Members$vacate_date %>% as.numeric() %>% as.Date() #changing to numeric first and then to date


Members <- Members %>% rowwise %>%  mutate(vacate_date = ifelse(is.character(vacate_date), term_end, vacate_date ) )
Members <- Members %>% rowwise %>%  mutate(vacate_date = ifelse(grep(any(LETTERS),vacate_date), term_end, vacate_date ) )

filter(Members, is.na(Members$term_end))
filter(Members, s_no==153)
uniquestates<- unique(Members$state)
uniqueParties<-unique(Members$party)
print(unique(Members$vacate_reason))
filter(Members, is.na(Members$vacate_date))
filter(Members, is.na(Members$term_start))
filter(Members, is.na(Members$term_end))

setwd("C:/Users/Mufti Taha Shah/Documents/GitHub/Rajya-Sabha")
read.csv("all_normalized_party_names.csv")
NormalizedParties<- read.csv("all_normalized_party_names.csv")
PartiesL<- unique(NormalizedParties$Expanded.Party.Name)
for (val in Members$vacate_date) 
  +     {if (val == "Retirement" |"Reorganisation of the State" |"" |"Resignation")
    +         Members$vacate_date = Members$term_end} #running the condition with a for loop

for (val in Members$vacate_date) {
  +     if(val == "Retirement")
    +         Members$vacate_date = Members$term_end
    +         } #running the condition with just retirement
library(data.table)
NormalizedParties<- fread("/Users/salonibhogale/tcpd_data/all_normalized_party_names.csv")
NormalizedParties = NormalizedParties[Election_Type=='GE',]
NormalizedParties = NormalizedParties[Year>=1980 & Year < 1990,]
PartiesL<- unique(NormalizedParties$Expanded.Party.Name)
library(lubridate)
Members = data.table(Members)
Members$term_start = dmy(Members$term_start)
Members$term_end = dmy(Members$term_end)
Members = Members[year(term_start)>=1980 & year(term_start)<1990,]
unique_parties_rs = unique(Members$party)
# 1980 to 1990 

Members8 <- Members[year(term_start)>=1980 & year(term_start)<1990,]
unique_parties_rs8 = unique(Members8$party) #unique parties in the 1980's

Members7 <- Members[year(term_start)>=1970 & year(term_start)<1980,]
unique_parties_rs7 = unique(Members7$party) #unique parties in the 1970's

Members5 <- Members[year(term_start)>=1950 & year(term_start)<1960,]
unique_parties_rs5 = unique(Members$party) #unique parties in the 1950's

Members6 <- Members[year(term_start)>=1960 & year(term_start)<1970,]
unique_parties_rs6 = unique(Members6$party) #unique parties in the 1960's

NormalizedParties = NormalizedParties[NormalizedParties$Election_Type=='GE',]
NormalizedParties8 = NormalizedParties[NormalizedParties$Year>=1980 & NormalizedParties$Year < 1990,]
NormalizedParties8<- unique(NormalizedParties8$Expanded.Party.Name)
