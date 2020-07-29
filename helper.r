#Left to do:
#have to clean four dates where century changes for vacation date changes
#have to clean NA's for vacation dates, reasons, 
#terms starts and term ends - Kshatriya Shri Shivpratap
#Normalized Parties - Others, Blanks

clean_members_data <- function(allmembers){
  allmembers <- separate(allmembers,term_from_to, c("term_start", "term_end"), " ", TRUE, TRUE) #to separate the term column
  allmembers$vacation_date =dmy(allmembers$vacation_date_reason)
  allmembers$vacation_reason = gsub("[^[:alpha:]]", "", allmembers$vacation_date_reason)
  # print(allmembers[is.na(vacation_reason),])
  # print(unique(allmembers$vacation_reason))
  allmembers$term_start = dmy(allmembers$term_start)
  allmembers$term_end = dmy(allmembers$term_end)
  for(i in 1:nrow(allmembers)){
    if(allmembers$vacation_reason[i]=='Retirement'){
      allmembers$vacation_date[i] = allmembers$term_end[i]
    }
  }
  print(allmembers[is.na(vacation_date),]) #20 entries where vacation date is not given, reaons include:Reorganisation of the State, Resignation, Death, Seat declared vacant 
  return(allmembers)
}

allmembers[vacation_date_reason == 04/03/57, ]$vacation_date = 1957-03-04	
allmembers[vacation_date_reason == 12/03/57, ]$vacation_date = 1957-03-12		
allmembers[vacation_date_reason == "12/03/57", ]$vacation_date = "1957-03-12"


get_50s_data <- function(allmembers){
  Members5 <- allmembers[year(term_start)>=1950 & year(term_start)<1960,]
  
  
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
  
  Members5[name_of_member=='Kamta Singh Shri',]$party_normalized='Swatantra Party'
  Members5[name_of_member=='Kishen Chand Shri',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Kulkarni Shri Gajanan Ramrao',]$party_normalized='Congress'
  Members5[name_of_member=='Manjuran Shri Mathai',]$party_normalized='Socialist Party'
  Members5[name_of_member=='Misra Shri Chandragopal Gajadharprasad.',]$party_normalized='Kisan Mazdoor Praja Party'
  Members5[name_of_member=='Mohanty Shri Surendra',]$party_normalized='Congress (I)'
  Members5[name_of_member=='Mookerjee Dr. Radha Kumud',]$party_normalized='Nominated'
  Members5[name_of_member=='Munshi Shri Arman Ali',]$party_normalized='Ganatantrik Sangh'
  Members5[name_of_member=='Narendra Deva Shri',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Niranjan Singh Shri',]$party_normalized='Congress'
  Members5[name_of_member=='Paliwal Shri Tika Ram',]$party_normalized='Congress'
  Members5[name_of_member=='Parameswaran Shri B.',]$party_normalized='Congress'
  Members5[name_of_member=='Parvathi Krishnan Smt.',]$party_normalized='Communist Party'
  Members5[name_of_member=='Patel Shri Dahyabhai V.',]$party_normalized='Swatantra Party'
  Members5[name_of_member=='Patel Shri Harihar',]$party_normalized='Ganatantra Parishad'
  Members5[name_of_member=='Sinha Shri Ganga Sharan',]$party_normalized='Nominated'
 
  Members5[name_of_member=='Ansari Shri Faridul Haq',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Basavapunnaiah Shri Makkineni',]$party_normalized='Commmunist Party of India'
  Members5[name_of_member=='Bhanj Deo Shri Prafulla Chandra',]$party_normalized='Ganatantra Parishad'
  Members5[name_of_member=='Bose Dr. Atindra Nath',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Dave Shri Rohit Manushankar',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Deshmukh Shri Narsinghrao Balbhimrao',]$party_normalized='Peasants and Workers Party'
  Members5[name_of_member=='Dube Shri Baij Nath',]$party_normalized='Socialist Party'
  Members5[name_of_member=='Dwivedy Shri Surendranath',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Ghose Shri Bimal Comar',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Gour Dr. Raj Bahadur',]$party_normalized='Commmunist Party of India'
  Members5[name_of_member=='Khobragade Shri Bhaurao Dewaji',]$party_normalized='Republican Party of India'
  Members5[name_of_member=='Patnaik Shri Dibakar',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Rajah Shri H. D.',]$party_normalized='Republican Party of India'
  Members5[name_of_member=='Rath Shri Abhimanyu',]$party_normalized='Ganatantra Parishad'
  Members5[name_of_member=='Reddy Shri C. G.',]$party_normalized='Socialist Party'
  Members5[name_of_member=='Reddy Shri Mulka Govinda',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Singh Shri Devendra Prasad',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Sinha Shri Maheswar Prasad Narain',]$party_normalized='Socialist Party'
  Members5[name_of_member=='Suryanarayana Shri Kommareddi',]$party_normalized='Kisan Mazdoor Praja Party'
  Members5[name_of_member=='Venkatanarayana Shri Pydah',]$party_normalized='Praja Socialist Party'
  Members5[name_of_member=='Vijaya Raje Kunwarani',]$party_normalized='Janata Party'
  
  return(Members5)
}

get_60s_data <- function(allmembers){
  Members6 <- allmembers[year(term_start)>=1960 & year(term_start)<1970,]
  
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

  
  Members6[name_of_member=='Kapoor Shri Giriraj Kishore',]$party_normalized='Jan Sangh'
  Members6[name_of_member=='Khandekar Shri Rameshehandra Shankarrao',]$party_normalized='Praja Socialist Party'
  Members6[name_of_member=='Kureel Shri Piare Lall alias Piare Lall Talib Unnavi',]$party_normalized='Congress(I)'
  Members6[name_of_member=='Lal Prof. Mukut Behari',]$party_normalized='Praja Socialist Party'
  Members6[name_of_member=='Limaye Shri Shripad Krishna',]$party_normalized='Lal Nishan Organisation'
  Members6[name_of_member=='Misra Shri Lokanath',]$party_normalized='Janta Party'
  Members6[name_of_member=='Mohta Shri Mahendra Kumar',]$party_normalized='Swatantra Party'
  Members6[name_of_member=='Murahari Shri Godey',]$party_normalized='Independent'
  Members6[name_of_member=='Nafisul Hasan Shri',]$party_normalized='Congress'
  Members6[name_of_member=='Naicker Shri M.A. Manickavelu',]$party_normalized='Congress'
  Members6[name_of_member=='Naidu Ms. M.L. Mary',]$party_normalized='Congress'
  Members6[name_of_member=='Narayan Shri M.D.',]$party_normalized='Independent'
  Members6[name_of_member=='Narayanappa Shri Sanda',]$party_normalized='Congress (O)'
  Members6[name_of_member=='Nehru Smt. Uma',]$party_normalized='Congress'
  Members6[name_of_member=='Niranjan Singh Shri',]$party_normalized='Congress'
  Members6[name_of_member=='Oberoi Shri Mohan Singh',]$party_normalized='Bharatiya Kranti Dal'
  Members6[name_of_member=='Pahadia Shri Jagannath Prasad',]$party_normalized='Congress'
  Members6[name_of_member=='Parthasarathy Prof.(Smt) G.',]$party_normalized='Congress'
  Members6[name_of_member=='Parthasarathy Shri R.T.',]$party_normalized='SCongress (O)'
  Members6[name_of_member=='Patel Shri Dahyabhai V.',]$party_normalized='Swatantra Party'
  Members6[name_of_member=='Patel Ms. Maniben Vallabhbhai',]$party_normalized='Congress'
  Members6[name_of_member=='Patel Shri Sundarmani',]$party_normalized='Swatantra Party'
  Members6[name_of_member=='Patil Shri Udhavrao Sahebrao',]$party_normalized='Peasants and Workers Party'
  Members6[name_of_member=='Prithwi Nath Shri',]$party_normalized='Bharatiya Kranti Dal'
  Members6[name_of_member=='Ramaswamy Shri V. V.',]$party_normalized='Congress'
  Members6[name_of_member=='Reddy Shri J. C. Nagi',]$party_normalized='Congress (O)'
  Members6[name_of_member=='Reddy Shri Mulka Govinda',]$party_normalized='Congress'
  Members6[name_of_member=='Sinha Shri Ganga Sharan',]$party_normalized='Nominated'
  Members6[name_of_member=='Basavapunnaiah Shri Makkineni',]$party_normalized='Commmunist Party of India'
  Members6[name_of_member=='Chandrasekharan Shri K.',]$party_normalized='Socialist Party'
  Members6[name_of_member=='Dar Shri Abdul Ghani',]$party_normalized='Progressive Independent Party'
  Members6[name_of_member=='Desai Shri Dajiba Balwantrao',]$party_normalized='Peasants and Workers Party'
  Members6[name_of_member=='Gaikwad Shri B K',]$party_normalized='Republican Party of India'
  Members6[name_of_member=='Rajnarain Shri',]$party_normalized='Socialist Party'
  Members6[name_of_member=='Vajpayee Shri Atal Bihari',]$party_normalized='Jan Sangh'
    return(Members6)
  
}

get_70s_data <- function(allmembers){
  Members7 <- allmembers[year(term_start)>=1970 & year(term_start)<1980,]
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
  
  Members7[party=='JAN',]$party_normalized='Janata Party'
  Members7[party=='JP',]$party_normalized='Janata Party'
  Members7[name_of_member=='Mishra Shri Kalraj',]$party_normalized='Bhartiya Janata Party'
  
  
  Members7[name_of_member=='Khan Shri Ghayoor Ali',]$party_normalized='Lok Dal'
  Members7[name_of_member=='Lotha Shri Khyomo',]$party_normalized='Congress (I)'
  Members7[name_of_member=='Patel Shri Dahyabhai V.',]$party_normalized='Swatantra Party'
  Members7[name_of_member=='Saha Shri Surajmal',]$party_normalized='Jana Congress'
  Members7[name_of_member=='Bhabhra Shri Hari Shankar',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Chandrasekharan Shri K.',]$party_normalized='Socialist Party'
  Members7[name_of_member=='Deo Shri K.P. Singh',]$party_normalized='Bharatiya Lok Dal'
  Members7[name_of_member=='Dhulap Shri Krishnarao Narayan',]$party_normalized='Peasants and Workers Party'
  Members7[name_of_member=='Goray Shri N G',]$party_normalized='Socialist Party'
  Members7[name_of_member=='Gupta Shri Ram Lakhan Prasad',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Jagbir Singh Shri',]$party_normalized='Lok Dal'
  Members7[name_of_member=='Joshi Shri Jagannathrao',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Lakhan Singh Shri',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Mahavir Dr. Bhai',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Mandal Shri Bhupendra Narayan',]$party_normalized='Bharatiya Lok Dal'
  Members7[name_of_member=='Mathur Shri Jagdish Prasad',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Mohinder Kaur Smt.',]$party_normalized='Bhartiya Janata Party'
  Members7[name_of_member=='Tombi Shri Salam',]$party_normalized='Manipur People\'s Party'
  return(Members7) 
}

get_80s_data<-function(allmembers){
  Members8 <- allmembers[year(term_start)>=1980 & year(term_start)<1990,]
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
  Members8[name_of_member=='Lather Shri Mohinder Singh',]$party_normalized='JANATA DAL'
  Members8[name_of_member=='Matto Shri Ghulam Rasool',]$party_normalized='JAMMU & KASHMIR NATIONAL CONFERENCE'
  Members8[party=='JD(S)',]$party_normalized='Janata Dal (Secular)'
  Members8[party=='SSP',]$party_normalized='SIKKIMSAGRAM PARISHAD'
  
  Members8[name_of_member=='Mahabir Prasad Dr.',]$party_normalized='JANTA PARTY'
  Members8[name_of_member=='Mahishi Dr.(Smt.) Sarojini',]$party_normalized='JANTA PARTY'
  Members8[name_of_member=='Reddy Shri P. Babul',]$party_normalized='JANTA PARTY'
  Members8[name_of_member=='Yadav Shri Hukmdeo Narain',]$party_normalized='JANTA PARTY'
  return(Members8)
}

get_90s_data <-function(allmembers){
  Members9 <- allmembers[year(term_start)>=1990 & year(term_start)<2000,]
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
  Members9[name_of_member=='Alphonse Shri S.Peter',]$party_normalized='TAMIL MAANILA CONGRESS (MOOPANAR)'
  Members9[name_of_member=='Moopanar Shri G. K.',]$party_normalized='TAMIL MAANILA CONGRESS (MOOPANAR)'
  Members9[party=='SP',]$party_normalized='SAMAJWADI PARTY'
  Members9[party=='SSP',]$party_normalized='SIKKIM SANGRAM PARISHAD'
  Members9[party=='SS',]$party_normalized='SHIVSENA'
  return(Members9)
  
}

get_2000s_data <-function(allmembers){
  Members0 <- allmembers[year(term_start)>=2000 & year(term_start)<2010,]
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
  return(Members0)
}

get_2010s_data <-function(allmembers){
  Members1 <- allmembers[year(term_start)>=2010 & year(term_start)<2020,]
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
  return(Members1)
}


get_year_list <- function(data){
  years<- seq(as.Date("1952-01-01"), as.Date("2020-01-01"), by="years")
  years<- data.table(years)
  data$yearlist= ''
  for (i in 1:nrow(data)){
    year_list = ''
    for (j in 1: nrow(years)){
      if (years$years[j] > data$term_start[i] && years$years[j] < data$vacation_date[i])
      {
        year_list = paste0(year_list,",",format(years$years[j], "%Y"))
      }
    }
    data$yearlist[i] = year_list
  }
  return(data)
}


get_year_wise_count <- function(data){
  years<- seq(as.Date("1952-01-01"), as.Date("2020-01-01"), by="years")
  years<- data.table(years)
  count_table = data.table()
  count_table$year= format(years$years,"%Y")
  count_table$nominated_members = 0
  for(i in 1:nrow(data)){
    for(j in 1:nrow(years)){
      if(grepl(format(years$years[j], "%Y"),data$yearlist[i])){
        count_table[year == format(years$years[j], "%Y"),]$nominated_members[1]=count_table[year == format(years$years[j], "%Y"),]$nominated_members[1]+1
      }
    }
    
  }
  return(count_table)
}
