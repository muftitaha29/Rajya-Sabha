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
#Coudn't do -CONG (S),O,JAN,JD(U)
#Two entries - Cong(S), O
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

Members8[member_name=='Mahabir Prasad Dr.',]$party='JANTA PARTY'
Members8[member_name=='Mahishi Dr.(Smt.) Sarojini',]$party='JANTA PARTY'
Members8[member_name=='Reddy Shri P. Babul',]$party='JANTA PARTY'
Members8[member_name=='Yadav Shri Hukmdeo Narain',]$party='JANTA PARTY'

#Coudn't do - CONG (O) - 10/15 entries, BJP(isn't in the Normalized set), Socialist,Communist 
#Basavapunnaiah Shri Makkineni- State from which I am elected:	Madras, My Political Party:	Communist - pdf also says Communist
#All the members with Cong(o) have the same thing in the website pdfs, one or two have independent or just Congress.
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
Members9[party=='JD',]$party='JANATA DAL'
Members9[party=='BJP',]$party='BHARATIYA JANATA PARTY'
Members9[party=='DMK',]$party='DRAVIDA MUNNETRA KAZHAGAM'
Members9[party=='IND.',]$party='INDEPENDENT'
Members9[party=='CPI',]$party='COMMUNIST PARTY OF INDIA'
Members9[party=='NOM',]$party='NOMINATED'
Members9[party=='RJD',]$party='RASHTRIYA JANATA DAL'
Members9[party=='BSP',]$party='BAHUJAN SAMAJ PARTY'
Members9[party=='AIADMK',]$party='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members9[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members9[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members9[party=='FB',]$party='FORWARD BLOCK'
Members9[party=='AIFB',]$party='ALL INDIA FORWARD BLOCK'
Members9[party=='RSP',]$party='RASHTRIYA SAMDARSHI PARTY'
Members9[party=='TDP',]$party='TELUGU DESAM PARTY'
Members9[party=='ML',]$party='MUSLIM LEAGUE'
Members9[party=='JP',]$party='JANATA PARTY'
Members9[party=='JD(S)',]$party='JANATA DAL (SAMAJWADI)'
Members9[party=='J&KNC',]$party='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members9[party=='HVP',]$party='HARYANA VIKAS PARTY'
Members9[party=='BJD',]$party='BIJU JANATA DAL'
Members9[party=='HVC',]$party='HIMACHAL VIKASH CONGRESS'
Members9[party=='JMM',]$party='JHARKHAND MUKTI MORCHA'
Members9[party=='HSPDP',]$party='HILL STATE PEOPLES DEMOCRATIC PARTY'

Members9[party=='AGP',]$party='ASOM GANA PARISHAD'
Members9[member_name=='Alphonse Shri S.Peter',]$party='TAMIL MAANILA CONGRESS (MOOPANAR)'
Members9[member_name=='Moopanar Shri G. K.',]$party='TAMIL MAANILA CONGRESS (MOOPANAR)'
Members9[party=='SP',]$party='SAMAJWADI PARTY'
Members9[party=='SSP',]$party='SIKKIM SANGRAM PARISHAD'
Members9[party=='SS',]$party='SHIVSENA'

#Members0[party=='NCP',]$party='Nationalist Congress Party'- National or Nationalist?
#ADIK , SHRI GOVINDRAO : B .A . (Hons ) , LL .B . ; I.N.C. (Maharashtra); 
#Chavan Shri Vasant, Meghe Shri Datta PATEL, SHRI MUKESH R.: N.C.P. (Maharashtra);
#Mohite-Patil Shri Ranjitsinh Vijaysinh,Sangma Shri Thomas,Sule Smt. Supriya,Trivedi Dr. Yogendra P.,Waghmare Dr. Janardhan - Couldn't find
#TARIQ, SHRI A. M. : Congress (Jammu and Kashmir);
# Members0[party=='LJP',]$party='' - Ali Shri Sabir,Bihar,LJP
#Members0[party=='RSP',]$party=''- PREMACHANDRAN, SHRI N. K. : B.Sc., LL.B.; R.S.P. (Kerala);
#Members0[party=='CONG(I)',]$party=''


Members0[party=='J&KNC',]$party='Jammu & Kashmir National Conference'
Members0[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members0[party=='BJP',]$party='BHARTIYA JANTA PARTY'
Members0[party=='CPI',]$party='COMMUNIST PARTY OF INDIA'
Members0[party=='IND.',]$party='INDEPENDENT'
Members0[party=='NCP',]$party='Nationalist Congress Party'
Members0[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members0[party=='TDP',]$party='Telugu Desam Party'
Members0[party=='JD(U)',]$party='Janata Dal (United)'
Members0[party=='AIADMK',]$party='All India Anna Dravida Munnetra Kazhagam'
Members0[party=='PDP',]$party='Peoples Democratic Party'
Members0[party=='SAD',]$party='Shiromani Akali Dal'
Members0[party=='NOM',]$party='NOMINATED'
Members0[party=='RJD',]$party='Rashtriya Janata Dal'
Members0[party=='AIFB',]$party='ALL INDIA FORWARD BLOCK'
Members0[party=='AITC',]$party='All India Trinamool Congress'
Members0[party=='INLD',]$party='Indian National Lok Dal'
Members0[party=='DMK',]$party='Dravida Munnetra Kazhagam'
Members0[party=='SBP',]$party='Swatantra Bharat Paksha'
 Members0[party=='MNF',]$party='MIZO NATIONAL FRONT'
Members0[party=='RLD',]$party='Rashtriya Lok Dal'
Members0[party=='JMM',]$party='JHARKHAND MUKTI MORCHA'
Members0[party=='JD(S)',]$party='Janata Dal  (Secular)'
Members0[party=='ML',]$party='MUSLIM LEAGUE'

Members0[party=='SP',]$party='Samajwadi Party'
Members0[party=='BSP',]$party='Bahujan Samaj Party'
Members0[party=='SS',]$party='Shiv Sena'
Members0[party=='RSP',]$party='Revolutionary Socialist Party'
Members0[party=='SDF',]$party='Sikkim Democratic Front'
Members0[party=='BJD',]$party='Biju Janata Dal'
Members0[party=='ABLC',]$party='Akhil Bhartiya Loktantrik Congress'
Members0[party=='SAMATA',]$party='Samata Party'
Members0[party=='N.P.F.',]$party='Nagaland Peoples Front'

# Members1[party=='NCP',]$party=''
# Members1[party=='N.P.F.',]$party=''
Members1[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members1[party=='KC(M)',]$party='Kerala Congress (M)'
Members1[party=='NOM.',]$party='NOMINATED'
Members1[party=='BSP',]$party='Bahujan Samaj Party'
Members1[party=='SP',]$party='Samajwadi Party'
Members1[party=='JD(U)',]$party='Janata Dal(United)'
Members1[party=='IND.',]$party='INDEPENDENT'
Members1[party=='AIADMK',]$party='All India Anna Dravida Munnetra Kazhagam'
Members1[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members1[party=='AITC',]$party='All India Trinamool Congress'
Members1[party=='BJP',]$party='Bharatiya Janata Party'
Members1[party=='BJD',]$party='Bharatiya Janta Dal'
Members1[party=='RJD',]$party='Rashtriya Janata Dal'
Members1[party=='DMK',]$party='Dravida Munnetra Kazhagam'
Members1[party=='INLD',]$party='Indian National Lok Dal'
Members1[party=='CPI',]$party='Communist Party of India'
Members1[party=='TDP',]$party='Telugu Desam Party'
Members1[party=='JMM',]$party='JHARKHAND MUKTI MORCHA'

# Members5[party=='SCF',]$party=''
Members5[party=='SCF',]$party='Scheduled Castes Federation'
# Members5[party=='JHKP',]$party='' - two entries
Members5[party=='JHKP',]$party='Jharkand Party'
# Members5[party=='HM',]$party='' - Angre Sardar Chandroji Sambhaji Rao	
Members5[party=='HM',]$party='Hindu Mahasabha'
# Members5[party=='FB(M)',]$party=''- Banerjee Shri Satyapriya	West Bengal	
Members5[party=='FB(M)',]$party='All India Forward Bloc (Marxist)'
# Members5[party=='Communist',]$party=''- Basavapunnaiah Shri Makkineni	- BASAVAPUNNAIAH, SHRI MAKKINENI : Communist (Andhra Pradesh);
# Members5[party=='O',]$party='' - 10/15 entries, have to be checked one by one
# Members5[party=='RPI',]$party='' - two entries -RAJAH, SHRI H. D. : R.P.I. (Madras now Tamil Nadu);KHOBRAGADE, SHRI BHAURAO DEWAJI : B.A., Barrister-atLaw; R .P.I .(Khobragade ) (Maharashtra )
# Members5[party=='Socialist',]$party='' - two entries: REDDY, SHRI C. GOPALA KRISHNAMOORTHY : Marine Engineer; Socialist Party (Mysore now Karnataka);
#SINHA, SHRI MAHESWAR PRASAD NARAIN : Socialist Party (Bihar);
# Members5[party=='JS',]$party='' - one entry: SINGH, RAJA AJIT PRATAP : Bhartiya Jan Sangh (Uttar Pradesh);
# Members5[party=='KMPP',]$party=''- one entry: SURYANARAYANA, SHRI KOMMAREDDI : Congress (Andhra Pradesh);
# Members5[party=='JAN',]$party=''- one entry:VIJAYA RAJE, KUNWARANI : Janata Party (Bihar);
Members5[party=='Congress',]$party='INDIAN NATIONAL CONGRESS'
Members5[party=='CPI',]$party='Communist Party of India'
Members5[party=='J&KNC',]$party='Jammu & Kashmir National Conference'
Members5[party=='NOM',]$party='NOMINATED'
Members5[party=='IND.',]$party='INDEPENDENT'
Members5[party=='INC',]$party='INDIAN NATIONAL CONGRESS'
Members5[party=='ML',]$party='MUSLIM LEAGUE'
Members5[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'

#Members7[party=='SSP',]$party=''
Members7[party=='AIADMK',]$party='ALL INDIA ANNA DRAVIDA MUNNETRA KAZHAGAM'
Members7[party=='ML',]$party='MUSLIM LEAGUE'
Members7[party=='NOM.',]$party='NOMINATED'
Members7[party=='JS',]$party='Jana Sangh'
Members7[party=='CPI',]$party='COMMUNIST PARTY OF INDIA'
Members7[party=='IND.',]$party='INDEPENDENT'
Members7[party=='RSP',]$party='Revolutionary Socialist Party'
Members7[party=='DSP',]$party='Democratic Socialist Party'
Members7[party=='AD',]$party='Akali Dal'
Members7[party=='FB',]$party='FORWARD BLOCK'
Members7[party=='CPI(M)',]$party='COMMUNIST PARTY OF INDIA (MARXIST)'
Members7[party=='DMK',]$party='DRAVIDA MUNNETRA KAZHAGAM'
Members7[party=='RPI',]$party='Republican Party of India'
Members7[party=='JD',]$party='Janata Dal'
Members7[party=='J&KNC',]$party='JAMMU & KASHMIR NATIONAL CONFERENCE'
Members7[party=='LD',]$party='Lok Dal'
Members7[party=='BKD',]$party='Bharatiya Kranti Dal'
Members7[party=='SAD',]$party='SHIROMANI AKALI DAL'
Members7[party=='BLD',]$party='Bharatiya Lok Dal'
Members7[party=='PSP',]$party='Praja Socialist Party'