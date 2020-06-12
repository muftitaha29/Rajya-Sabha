setwd('~/Documents/GitHub/Rajya-Sabha/')
library(tidyr)
library(dplyr)
Members <- read.csv("rs_member_terms.csv") # defining the data frame
Members <- separate(Members, vacate_reason, c("vacate_date", "vacate_reason"), "\n", TRUE, TRUE) #to separate the vacate reason into date & reason 
Members <- separate(Members,term, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column
Members <- Members %>% rowwise %>%  mutate(vacate_reason = ifelse(is.na(vacate_reason), vacate_date, vacate_reason ) ) #running the operation row wise

library(data.table)
Members<-data.table(Members)
print(Members[is.na(vacate_date),])
print(Members[is.na(term_start),])
print(Members[is.na(term_end),])
print(Members[is.na(state),])

uniquestates<- unique(Members$state)
print(unique(Members$vacate_reason))



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

NormalizedParties6 = NormalizedParties[NormalizedParties$Year>=1960 & NormalizedParties$Year < 1970,]
NormalizedParties6 = unique(NormalizedParties6$Expanded.Party.Name)

NormalizedParties7 = NormalizedParties[NormalizedParties$Year>=1970 & NormalizedParties$Year < 1980,]
NormalizedParties7 = unique(NormalizedParties7$Expanded.Party.Name)


Members8[party=='CONG(I)',]$party='INDIAN NATIONAL CONGRESS (I)'
Members8[party=='BJP',]$party='BHARATIYA JANTA PARTY'
Members8[party=='JD',]$party='JANATA DAL'
Members8[party=='AIADMK',]$party='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members8[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members8[party=='NOM.',]$party='NOMINATED'
Members8[party=='Congress',]$party='INDIAN NATIONAL CONGRESS'
Members8[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members8[party=='CPI',]$party='COMMUNIST PARTY OF INDIA'
Members8[party=='DMK',]$party='DRAVIDA MUMMETRA KAZHAGAM'
Members8[party=='JP',]$party='JANTA PARTY'
Members8[party=='IND.',]$party='INDEPENDENT'
Members8[party=='LD',]$party='LOK DAL'
Members8[party=='UDF(N)',]$party='UNITED DEMOCRATIC FRONT'
Members8[party=='ML',]$party='MUSLIM LEAGUE'
Members8[party=='KC',]$party='KERALA CONGRESS'
Members8[party=='J&KNC',]$party='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members8[party=='SAD',]$party='SHIROMANI AKALI DAL'
#Coudn't do - AD,FB,CONG (S),RSP,AGP,TDP,O,JAN,JD(S),JD(U),SSP
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx Mistry Smt. Roda	- Party 'Other'
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx
#Pandit Ravi Shankar State -Nominated My Political Party:	Other
#Masood Shri Rasheed	: State from which I am elected:	Uttar Pradesh, My Political Party:	Indian National Congress
#Naik Shri R.S.	:State from which I am elected:	Karnataka, My Political Party:	Janata
Members8[party=='AD',]$party='AKALI DAL'
Members8[party=='FB',]$party='FORWARD BLOCK'
Members8[party=='RSP',]$party='REVOLUTIONARY SOCIALIST PARTY'
Members8[party=='AGP',]$party='Asom Gana Parishad'
Members8[party=='TDP',]$party='Telugu Desam Party'
Members8[party=='CONG(I)',]$party='INDIAN NATIONAL CONGRESS (I)'
Members8[member_name=='Lather Shri Mohinder Singh',]$party='JANATA DAL'
Members8[member_name=='Matto Shri Ghulam Rasool',]$party='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members8[party=='JD(S)',]$party='Janata Dal (Secular)'
Members8[party=='SSP',]$party='SIKKIMSAGRAM PARISHAD'

#Coudn't do - CONG (O), BJP(isn't in the Normalized set)
#Basavapunnaiah Shri Makkineni- State from which I am elected:	Madras, My Political Party:	Communist
Members6[party=='ML',]$party='MUSLIM LEAGUE'
Members6[party=='Congress',]$party='INDIAN NATIONAL CONGRESS'
Members6[party=='CPI',]$party='COMMUNIST PARTY OF INDIA'
Members6[party=='NOM.',]$party='NOMINATED'
Members6[party=='DMK',]$party='DRAVIDA MUNITRA KAZHAGAM'
Members6[party=='PSP',]$party='PRAJA SOCIALIST PARTY'
Members6[party=='Swatantra',]$party='SWATANTRA PARTY'
Members6[party=='IND.',]$party='INDEPENDENT'
Members6[party=='SSP',]$party='SAMYUKTA SOCIALIST PARTY'
Members6[party=='FB',]$party='FORWARD BLOCK'
Members6[party=='JS',]$party='JAN SANGH'
Members6[party=='AD',]$party='AKALI DAL'
Members6[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members6[party=='BKD',]$party='BHARATIYA KRANTI DAL'
Members6[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members6[party=='RPI',]$party='REPUBLICAN PARTY OF INDIA'
Members6[party=='CONG(I)',]$party='INDIAN NATIONAL CONGRESS (I)'#INDIAN NATIONAL CONGRESS (I) wasn't in the Normalized dataset
Members6[party=='RSP',]$party='REVOLUTIONARY SOCIALIST PARTY'
Members6[party=='SAD',]$party='SHIROMANI AKALI DAL'