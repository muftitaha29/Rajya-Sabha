setwd("C:/Users/Mufti Taha Shah/Desktop/Ashoka/TCPD")
read.csv("rs_member_terms.csv")
library(tidyr)
Members <- read.csv("rs_member_terms.csv") # defining the data frame
Members <- separate(Members, vacate_reason, c("vacate_date", "vacate_reason"), "\n", TRUE, TRUE) #to separate the vacate reason into date & reason 
Members <- separate(Members,term, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column
Members <- mutate(Members,vacate_reason = ifelse(is.na(vacate_reason), vacate_date, vacate_reason ) )
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

