setwd('~/Documents/GitHub/Rajya-Sabha/')
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)

# load files 
allmembers<- fread("all_rs_members_scraped.csv")
allmembers <- clean_members_data(allmembers)
Members5<-get_50s_data(allmembers)
Members6<-get_60s_data(allmembers)
Members7<-get_70s_data(allmembers)
Members8<-get_80s_data(allmembers)
Members9<-get_90s_data(allmembers)
Members0<-get_2000s_data(allmembers)
Members1<-get_2010s_data(allmembers)
Members_Cleaned<- rbind(Members5, Members6, Members7, Members8, Members9, Members0, Members1)
Members_Cleaned$party_normalized<- toupper(Members_Cleaned$party_normalized)
fwrite(Members_Cleaned,'rs_cleaned.csv')

states<- unique(allmembers$state)
#Madhya Bharat	Congress	1960-04-03; Ajmer and Coorg;Hathi Shri Jai Sukh Lal	Saurashtra	Congress	1968-04-03; 6 members with 'Others'; Patiala and East Punjab States Union; 
#Manipur:-Manipur should be a centrahy-administered territory for the time being.
#Bilaspur and Himachal Pradesh	



# unique_parties_rs5 = unique(Members5$party) #unique parties in the 1950's
# 
# NormalizedParties<- read.csv("all_normalized_party_names.csv")
# unique_parties_rs6 = unique(Members6$party) #unique parties in the 1960's
# NormalizedParties6 = NormalizedParties[NormalizedParties$Year>=1960 & NormalizedParties$Year < 1970,]
# NormalizedParties6 = unique(NormalizedParties6$Expanded.Party.Name)
# 
# unique_parties_rs7 = unique(Members7$party) #unique parties in the 1970's
# NormalizedParties7 = NormalizedParties[NormalizedParties$Year>=1970 & NormalizedParties$Year < 1980,]
# NormalizedParties7 = unique(NormalizedParties7$Expanded.Party.Name)
# 
# unique_parties_rs8 = unique(Members8$party) #unique parties in the 1980's
# NormalizedParties8 = NormalizedParties[NormalizedParties$Year>=1980 & NormalizedParties$Year < 1990,]
# NormalizedParties8 = unique(NormalizedParties8$Expanded.Party.Name)
# 
# unique_parties_rs9 = unique(Members9$party) #unique parties in the 1990's
# NormalizedParties9 = NormalizedParties[NormalizedParties$Year>=1990 & NormalizedParties$Year < 2000,]
# NormalizedParties9 = unique(NormalizedParties9$Expanded.Party.Name)
# 
# unique_parties_rs0 = unique(Members0$party) #unique parties in the 2000's
# NormalizedParties0 = NormalizedParties[NormalizedParties$Year>=2000 & NormalizedParties$Year < 2010,]
# NormalizedParties0 = unique(NormalizedParties0$Expanded.Party.Name)
# 
# unique_parties_rs1 = unique(Members1$party) #unique parties in the 2010's
# NormalizedParties1 = NormalizedParties[NormalizedParties$Year>=2010 & NormalizedParties$Year < 2020,]
# NormalizedParties1 = unique(NormalizedParties1$Expanded.Party.Name)
