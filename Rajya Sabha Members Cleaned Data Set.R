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
unique_parties_rs5 = unique(Members5$party) #unique parties in the 1950's

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

Members8$party_normalized <- ""
Members8[party=='CONG(I)',]$party_normalized='INDIAN NATIONAL CONGRESS (I)'
Members8[party=='BJP',]$party_normalized='BHARATIYA JANTA PARTY'
Members8[party=='JD',]$party_normalized='JANATA DAL'
Members8[party=='AIADMK',]$party_normalized='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members8[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members8[party=='NOM.',]$party_normalized='NOMINATED'
Members8[party=='Congress',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members8[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members8[party=='CPI',]$party_normalized='COMMUNIST PARTY OF INDIA'
Members8[party=='DMK',]$party_normalized='DRAVIDA MUMMETRA KAZHAGAM'
Members8[party=='JP',]$party_normalized='JANTA PARTY'
Members8[party=='IND.',]$party_normalized='INDEPENDENT'
Members8[party=='LD',]$party_normalized='LOK DAL'
Members8[party=='UDF(N)',]$party_normalized='UNITED DEMOCRATIC FRONT'
Members8[party=='ML',]$party_normalized='MUSLIM LEAGUE'
Members8[party=='KC',]$party_normalized='KERALA CONGRESS'
Members8[party=='J&KNC',]$party_normalized='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members8[party=='SAD',]$party_normalized='SHIROMANI AKALI DAL'
#Coudn't do -CONG (S),O,JAN,JD(U)
#Two entries - Cong(S), O
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx Mistry Smt. Roda	- Party 'Other'
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx
#Pandit Ravi Shankar State -Nominated My Political Party:	Other
#Masood Shri Rasheed	: State from which I am elected:	Uttar Pradesh, My Political Party:	Indian National Congress
#Naik Shri R.S.	:State from which I am elected:	Karnataka, My Political Party:	Janata
Members8[party=='AD',]$party_normalized='AKALI DAL'
Members8[party=='FB',]$party_normalized='FORWARD BLOCK'
Members8[party=='RSP',]$party_normalized='REVOLUTIONARY SOCIALIST PARTY'
Members8[party=='AGP',]$party_normalized='Asom Gana Parishad'
Members8[party=='TDP',]$party_normalized='Telugu Desam Party'
Members8[party=='CONG(I)',]$party_normalized='INDIAN NATIONAL CONGRESS (I)'
Members8[member_name=='Lather Shri Mohinder Singh',]$party_normalized='JANATA DAL'
Members8[member_name=='Matto Shri Ghulam Rasool',]$party_normalized='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members8[party=='JD(S)',]$party_normalized='Janata Dal (Secular)'
Members8[party=='SSP',]$party_normalized='SIKKIMSAGRAM PARISHAD'

Members8[member_name=='Mahabir Prasad Dr.',]$party_normalized='JANTA PARTY'
Members8[member_name=='Mahishi Dr.(Smt.) Sarojini',]$party_normalized='JANTA PARTY'
Members8[member_name=='Reddy Shri P. Babul',]$party_normalized='JANTA PARTY'
Members8[member_name=='Yadav Shri Hukmdeo Narain',]$party_normalized='JANTA PARTY'

Members6$party_normalized <- ""
#Coudn't do - CONG (O) - 10/15 entries, BJP(isn't in the Normalized set), Socialist,Communist 
#Basavapunnaiah Shri Makkineni- State from which I am elected:	Madras, My Political Party:	Communist - pdf also says Communist
#All the members with Cong(o) have the same thing in the website pdfs, one or two have independent or just Congress.
Members6[party=='ML',]$party_normalized='MUSLIM LEAGUE'
Members6[party=='Congress',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members6[party=='CPI',]$party_normalized='COMMUNIST PARTY OF INDIA'
Members6[party=='NOM.',]$party_normalized='NOMINATED'
Members6[party=='DMK',]$party_normalized='DRAVIDA MUNITRA KAZHAGAM'
Members6[party=='PSP',]$party_normalized='PRAJA SOCIALIST PARTY'
Members6[party=='Swatantra',]$party_normalized='SWATANTRA PARTY'
Members6[party=='IND.',]$party_normalized='INDEPENDENT'
Members6[party=='SSP',]$party_normalized='SAMYUKTA SOCIALIST PARTY'
Members6[party=='FB',]$party_normalized='FORWARD BLOCK'
Members6[party=='JS',]$party_normalized='JAN SANGH'
Members6[party=='AD',]$party_normalized='AKALI DAL'
Members6[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members6[party=='BKD',]$party_normalized='BHARATIYA KRANTI DAL'
Members6[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members6[party=='RPI',]$party_normalized='REPUBLICAN PARTY OF INDIA'
Members6[party=='CONG(I)',]$party_normalized='INDIAN NATIONAL CONGRESS (I)'#INDIAN NATIONAL CONGRESS (I) wasn't in the Normalized dataset
Members6[party=='RSP',]$party_normalized='REVOLUTIONARY SOCIALIST PARTY'
Members6[party=='SAD',]$party_normalized='SHIROMANI AKALI DAL'
#Rajnarain Shri	- pdf says Janata party
#Chandrasekharan Shri K.- Socialist Party (Kerala)
Members9 <- Members[year(term_start)>=1990 & year(term_start)<2000,]
unique_parties_rs9 = unique(Members9$party) #unique parties in the 1990's

Members0 <- Members[year(term_start)>=2000 & year(term_start)<2010,]
unique_parties_rs0 = unique(Members0$party) #unique parties in the 2000's

Members1 <- Members[year(term_start)>=2010 & year(term_start)<2020,]
unique_parties_rs1 = unique(Members1$party) #unique parties in the 2010's

NormalizedParties9 = NormalizedParties[NormalizedParties$Year>=1990 & NormalizedParties$Year < 2000,]
NormalizedParties9 = unique(NormalizedParties9$Expanded.Party.Name)

NormalizedParties0 = NormalizedParties[NormalizedParties$Year>=2000 & NormalizedParties$Year < 2010,]
NormalizedParties0 = unique(NormalizedParties0$Expanded.Party.Name)

NormalizedParties1 = NormalizedParties[NormalizedParties$Year>=2010 & NormalizedParties$Year < 2020,]
NormalizedParties1 = unique(NormalizedParties1$Expanded.Party.Name)

Members9$party_normalized <- ""
#Members9[party=='CONG(I)',]$party='INDIAN NATIONAL CONGRESS (I)' - No INDIAN NATIONAL CONGRESS (I) in the normalized set
#Members9[party=='TMC(M)',]$party='' - Trinamool Congress or TAMIL MAANILA CONGRESS (MOOPANAR) - Left with Khader Shri N. Abdul	& Natarajan Smt. Jayanthi
#Members9[party=='AGP',]$party=''
#Members9[party=='RSP',]$party='RASHTRIYA SAMDARSHI PARTY'- multiple partes abbreviate to RSP
#Members9[party=='SP',]$party='' - multiple partes abbreviate to SP
#Members9[party=='SAD',]$party=''- there are many factions of SAD
#Members9[party=='KC',]$party='' - KC has many factions
#Members9[party=='SS',]$party=''
#Members9[party=='Congress',]$party=''
#Members9[party=='JMM',]$party='JHARKHAND MUKTI MORCHA' - has many factions
#Members9[party=='NPC',]$party=''
#Yadav Shri Ish Dutt- Lok Dal or SP?
#PATEL, SHRI MUKESH R. : H.S.C.; N.C.P. (Maharashtra);
#SETHI, SHRI ANANTA : B.Sc.; I.N.C. (Orissa);
#QURESHI, SHRI ABDUL GAIYUR : M.A., LL.B., 'Sahitya Ratan'; I.N.C. (Madhya Pradesh);
Members9[party=='JD',]$party_normalized='JANATA DAL'
Members9[party=='BJP',]$party_normalized='BHARATIYA JANATA PARTY'
Members9[party=='DMK',]$party_normalized='DRAVIDA MUNNETRA KAZHAGAM'
Members9[party=='IND.',]$party_normalized='INDEPENDENT'
Members9[party=='CPI',]$party_normalized='COMMUNIST PARTY OF INDIA'
Members9[party=='NOM',]$party_normalized='NOMINATED'
Members9[party=='RJD',]$party_normalized='RASHTRIYA JANATA DAL'
Members9[party=='BSP',]$party_normalized='BAHUJAN SAMAJ PARTY'
Members9[party=='AIADMK',]$party_normalized='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members9[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members9[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members9[party=='FB',]$party_normalized='FORWARD BLOCK'
Members9[party=='AIFB',]$party_normalized='ALL INDIA FORWARD BLOCK'
Members9[party=='RSP',]$party_normalized='RASHTRIYA SAMDARSHI PARTY'
Members9[party=='TDP',]$party_normalized='TELUGU DESAM PARTY'
Members9[party=='ML',]$party_normalized='MUSLIM LEAGUE'
Members9[party=='JP',]$party_normalized='JANATA PARTY'
Members9[party=='JD(S)',]$party_normalized='JANATA DAL (SAMAJWADI)'
Members9[party=='J&KNC',]$party_normalized='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members9[party=='HVP',]$party_normalized='HARYANA VIKAS PARTY'
Members9[party=='BJD',]$party_normalized='BIJU JANATA DAL'
Members9[party=='HVC',]$party_normalized='HIMACHAL VIKASH CONGRESS'
Members9[party=='JMM',]$party_normalized='JHARKHAND MUKTI MORCHA'
Members9[party=='HSPDP',]$party_normalized='HILL STATE PEOPLES DEMOCRATIC PARTY'

Members9[party=='AGP',]$party_normalized='ASOM GANA PARISHAD'
Members9[member_name=='Alphonse Shri S.Peter',]$party_normalized='TAMIL MAANILA CONGRESS (MOOPANAR)'
Members9[member_name=='Moopanar Shri G. K.',]$party_normalized='TAMIL MAANILA CONGRESS (MOOPANAR)'
Members9[party=='SP',]$party_normalized='SAMAJWADI PARTY'
Members9[party=='SSP',]$party_normalized='SIKKIM SANGRAM PARISHAD'
Members9[party=='SS',]$party_normalized='SHIVSENA'


Members0$party_normalized <- ""
#Members0[party=='NCP',]$party='Nationalist Congress Party'- National or Nationalist?
#ADIK , SHRI GOVINDRAO : B .A . (Hons ) , LL .B . ; I.N.C. (Maharashtra); 
#Chavan Shri Vasant, Meghe Shri Datta PATEL, SHRI MUKESH R.: N.C.P. (Maharashtra);
#Mohite-Patil Shri Ranjitsinh Vijaysinh,Sangma Shri Thomas,Sule Smt. Supriya,Trivedi Dr. Yogendra P.,Waghmare Dr. Janardhan - Couldn't find
#TARIQ, SHRI A. M. : Congress (Jammu and Kashmir);
# Members0[party=='LJP',]$party='' - Ali Shri Sabir,Bihar,LJP
#Members0[party=='RSP',]$party=''- PREMACHANDRAN, SHRI N. K. : B.Sc., LL.B.; R.S.P. (Kerala);
#Members0[party=='CONG(I)',]$party=''


Members0[party=='J&KNC',]$party_normalized='Jammu & Kashmir National Conference'
Members0[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members0[party=='BJP',]$party_normalized='BHARTIYA JANTA PARTY'
Members0[party=='CPI',]$party_normalized='COMMUNIST PARTY OF INDIA'
Members0[party=='IND.',]$party_normalized='INDEPENDENT'
Members0[party=='NCP',]$party_normalized='Nationalist Congress Party'
Members0[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members0[party=='TDP',]$party_normalized='Telugu Desam Party'
Members0[party=='JD(U)',]$party_normalized='Janata Dal (United)'
Members0[party=='AIADMK',]$party_normalized='All India Anna Dravida Munnetra Kazhagam'
Members0[party=='PDP',]$party_normalized='Peoples Democratic Party'
Members0[party=='SAD',]$party_normalized='Shiromani Akali Dal'
Members0[party=='NOM',]$party_normalized='NOMINATED'
Members0[party=='RJD',]$party_normalized='Rashtriya Janata Dal'
Members0[party=='AIFB',]$party_normalized='ALL INDIA FORWARD BLOCK'
Members0[party=='AITC',]$party_normalized='All India Trinamool Congress'
Members0[party=='INLD',]$party_normalized='Indian National Lok Dal'
Members0[party=='DMK',]$party_normalized='Dravida Munnetra Kazhagam'
Members0[party=='SBP',]$party_normalized='Swatantra Bharat Paksha'
Members0[party=='MNF',]$party_normalized='MIZO NATIONAL FRONT'
Members0[party=='RLD',]$party_normalized='Rashtriya Lok Dal'
Members0[party=='JMM',]$party_normalized='JHARKHAND MUKTI MORCHA'
Members0[party=='JD(S)',]$party_normalized='Janata Dal  (Secular)'
Members0[party=='ML',]$party_normalized='MUSLIM LEAGUE'

Members0[party=='SP',]$party_normalized='Samajwadi Party'
Members0[party=='BSP',]$party_normalized='Bahujan Samaj Party'
Members0[party=='SS',]$party_normalized='Shiv Sena'
Members0[party=='RSP',]$party_normalized='Revolutionary Socialist Party'
Members0[party=='SDF',]$party_normalized='Sikkim Democratic Front'
Members0[party=='BJD',]$party_normalized='Biju Janata Dal'
Members0[party=='ABLC',]$party_normalized='Akhil Bhartiya Loktantrik Congress'
Members0[party=='SAMATA',]$party_normalized='Samata Party'
Members0[party=='N.P.F.',]$party_normalized='Nagaland Peoples Front'


Members1$party_normalized <- ""
# Members1[party=='NCP',]$party=''
# Members1[party=='N.P.F.',]$party=''
Members1[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members1[party=='KC(M)',]$party_normalized='Kerala Congress (M)'
Members1[party=='NOM.',]$party_normalized='NOMINATED'
Members1[party=='BSP',]$party_normalized='Bahujan Samaj Party'
Members1[party=='SP',]$party_normalized='Samajwadi Party'
Members1[party=='JD(U)',]$party_normalized='Janata Dal(United)'
Members1[party=='IND.',]$party_normalized='INDEPENDENT'
Members1[party=='AIADMK',]$party_normalized='All India Anna Dravida Munnetra Kazhagam'
Members1[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members1[party=='AITC',]$party_normalized='All India Trinamool Congress'
Members1[party=='BJP',]$party_normalized='Bharatiya Janata Party'
Members1[party=='BJD',]$party_normalized='Bharatiya Janta Dal'
Members1[party=='RJD',]$party_normalized='Rashtriya Janata Dal'
Members1[party=='DMK',]$party_normalized='Dravida Munnetra Kazhagam'
Members1[party=='INLD',]$party_normalized='Indian National Lok Dal'
Members1[party=='CPI',]$party_normalized='Communist Party of India'
Members1[party=='TDP',]$party_normalized='Telugu Desam Party'
Members1[party=='JMM',]$party_normalized='JHARKHAND MUKTI MORCHA'


Members5$party_normalized <- ""
# Members5[party=='SCF',]$party=''
Members5[party=='SCF',]$party_normalized='Scheduled Castes Federation'
# Members5[party=='JHKP',]$party='' - two entries
Members5[party=='JHKP',]$party_normalized='Jharkand Party'
# Members5[party=='HM',]$party='' - Angre Sardar Chandroji Sambhaji Rao	
Members5[party=='HM',]$party_normalized='Hindu Mahasabha'
# Members5[party=='FB(M)',]$party=''- Banerjee Shri Satyapriya	West Bengal	
Members5[party=='FB(M)',]$party_normalized='All India Forward Bloc (Marxist)'
# Members5[party=='Communist',]$party=''- Basavapunnaiah Shri Makkineni	- BASAVAPUNNAIAH, SHRI MAKKINENI : Communist (Andhra Pradesh);
# Members5[party=='O',]$party='' - 10/15 entries, have to be checked one by one
# Members5[party=='RPI',]$party='' - two entries -RAJAH, SHRI H. D. : R.P.I. (Madras now Tamil Nadu);KHOBRAGADE, SHRI BHAURAO DEWAJI : B.A., Barrister-atLaw; R .P.I .(Khobragade ) (Maharashtra )
# Members5[party=='Socialist',]$party='' - two entries: REDDY, SHRI C. GOPALA KRISHNAMOORTHY : Marine Engineer; Socialist Party (Mysore now Karnataka);
#SINHA, SHRI MAHESWAR PRASAD NARAIN : Socialist Party (Bihar);
# Members5[party=='JS',]$party='' - one entry: SINGH, RAJA AJIT PRATAP : Bhartiya Jan Sangh (Uttar Pradesh);
# Members5[party=='KMPP',]$party=''- one entry: SURYANARAYANA, SHRI KOMMAREDDI : Congress (Andhra Pradesh);
# Members5[party=='JAN',]$party=''- one entry:VIJAYA RAJE, KUNWARANI : Janata Party (Bihar);
Members5[party=='Congress',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members5[party=='CPI',]$party_normalized='Communist Party of India'
Members5[party=='J&KNC',]$party_normalized='Jammu & Kashmir National Conference'
Members5[party=='NOM',]$party_normalized='NOMINATED'
Members5[party=='IND.',]$party_normalized='INDEPENDENT'
Members5[party=='INC',]$party_normalized='INDIAN NATIONAL CONGRESS'
Members5[party=='ML',]$party_normalized='MUSLIM LEAGUE'
Members5[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'

Members7$party_normalized <- ""
#Members7[party=='SSP',]$party=''
Members7[party=='AIADMK',]$party_normalized='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members7[party=='ML',]$party_normalized='MUSLIM LEAGUE'
Members7[party=='NOM.',]$party_normalized='NOMINATED'
Members7[party=='JS',]$party_normalized='Jana Sangh'
Members7[party=='CPI',]$party_normalized='COMMUNIST PARTY OF INDIA'
Members7[party=='IND.',]$party_normalized='INDEPENDENT'
Members7[party=='RSP',]$party_normalized='Revolutionary Socialist Party'
Members7[party=='DSP',]$party_normalized='Democratic Socialist Party'
Members7[party=='AD',]$party_normalized='Akali Dal'
Members7[party=='FB',]$party_normalized='FORWARD BLOCK'
Members7[party=='CPI(M)',]$party_normalized='COMMUNIST PARTY OF INDIA (MARXIST)'
Members7[party=='DMK',]$party_normalized='DRAVIDA MUNNETRA KAZHAGAM'
Members7[party=='RPI',]$party_normalized='Republican Party of India'
Members7[party=='JD',]$party_normalized='Janata Dal'
Members7[party=='J&KNC',]$party_normalized='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members7[party=='LD',]$party_normalized='Lok Dal'
Members7[party=='BKD',]$party_normalized='Bharatiya Kranti Dal'
Members7[party=='SAD',]$party_normalized='SHIROMANI AKALI DAL'
Members7[party=='BLD',]$party_normalized='Bharatiya Lok Dal'
Members7[party=='PSP',]$party_normalized='Praja Socialist Party'

Members_Cleaned<- rbind(Members5, Members6, Members7, Members8, Members9, Members0, Members1)
	
