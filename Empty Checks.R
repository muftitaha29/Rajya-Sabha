Empty<- Members_Cleaned[party_normalized==""]
Empty<- subset(EMpty, party != 'CONG(I)')
Empty<- subset(EMpty, party != 'Congress')
Empty<- subset(EMpty, party != 'CONG(O)')
Empty<- subset(EMpty, party != 'CONG (S)')
Empty<- subset(EMpty, party != 'INC')
Empty<- subset(EMpty, party != 'JAN')

# Members5[party=='Communist',]$party=''- Basavapunnaiah Shri Makkineni	- BASAVAPUNNAIAH, SHRI MAKKINENI : Communist (Andhra Pradesh);
# Members5[party=='O',]$party='' - 10/15 entries, have to be checked one by one
# Members5[party=='RPI',]$party='' - two entries -RAJAH, SHRI H. D. : R.P.I. (Madras now Tamil Nadu);KHOBRAGADE, SHRI BHAURAO DEWAJI : B.A., Barrister-atLaw; R .P.I .(Khobragade ) (Maharashtra )
# Members5[party=='Socialist',]$party='' - two entries: REDDY, SHRI C. GOPALA KRISHNAMOORTHY : Marine Engineer; Socialist Party (Mysore now Karnataka);
#SINHA, SHRI MAHESWAR PRASAD NARAIN : Socialist Party (Bihar);
# Members5[party=='JS',]$party='' - one entry: SINGH, RAJA AJIT PRATAP : Bhartiya Jan Sangh (Uttar Pradesh);
# Members5[party=='KMPP',]$party=''- one entry: SURYANARAYANA, SHRI KOMMAREDDI : Congress (Andhra Pradesh);
# Members5[party=='JAN',]$party=''- one entry:VIJAYA RAJE, KUNWARANI : Janata Party (Bihar);
#Members6:
#Coudn't do - CONG (O) - 10/15 entries, BJP(isn't in the Normalized set), Socialist,Communist 
#Basavapunnaiah Shri Makkineni- State from which I am elected:	Madras, My Political Party:	Communist - pdf also says Communist
#All the members with Cong(o) have the same thing in the website pdfs, one or two have independent or just Congress.
#Members7[party=='SSP',]$party=''
#Members8:
#Coudn't do -CONG (S),O,JAN,JD(U)
#Two entries - Cong(S), O
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx Mistry Smt. Roda	- Party 'Other'
#https://rajyasabha.nic.in/rsnew/member_site/Main.aspx
#Pandit Ravi Shankar State -Nominated My Political Party:	Other
#Masood Shri Rasheed	: State from which I am elected:	Uttar Pradesh, My Political Party:	Indian National Congress
#Naik Shri R.S.	:State from which I am elected:	Karnataka, My Political Party:	Janata

#Members9[party=='CONG(I)',]$party='INDIAN NATIONAL CONGRESS (I)' - No INDIAN NATIONAL CONGRESS (I) in the normalized set
#Members9[party=='TMC(M)',]$party='' - Trinamool Congress or TAMIL MAANILA CONGRESS (MOOPANAR) - Left with Khader Shri N. Abdul	& Natarajan Smt. Jayanthi
#Members9[party=='RSP',]$party='RASHTRIYA SAMDARSHI PARTY'- multiple partes abbreviate to RSP
#Members9[party=='SAD',]$party=''- there are many factions of SAD
#Members9[party=='KC',]$party='' - KC has many factions
#Members9[party=='Congress',]$party=''
#Members9[party=='JMM',]$party='JHARKHAND MUKTI MORCHA' - has many factions
#Members9[party=='NPC',]$party=''
#Yadav Shri Ish Dutt- Lok Dal or SP?
#PATEL, SHRI MUKESH R. : H.S.C.; N.C.P. (Maharashtra);
#SETHI, SHRI ANANTA : B.Sc.; I.N.C. (Orissa);
#QURESHI, SHRI ABDUL GAIYUR : M.A., LL.B., 'Sahitya Ratan'; I.N.C. (Madhya Pradesh);

#Members0[party=='NCP',]$party='Nationalist Congress Party'- National or Nationalist?
#ADIK , SHRI GOVINDRAO : B .A . (Hons ) , LL .B . ; I.N.C. (Maharashtra); 
#Chavan Shri Vasant, Meghe Shri Datta PATEL, SHRI MUKESH R.: N.C.P. (Maharashtra);
#Mohite-Patil Shri Ranjitsinh Vijaysinh,Sangma Shri Thomas,Sule Smt. Supriya,Trivedi Dr. Yogendra P.,Waghmare Dr. Janardhan - Couldn't find
#TARIQ, SHRI A. M. : Congress (Jammu and Kashmir);
# Members0[party=='LJP',]$party='' - Ali Shri Sabir,Bihar,LJP
#Members0[party=='RSP',]$party=''- PREMACHANDRAN, SHRI N. K. : B.Sc., LL.B.; R.S.P. (Kerala);
#Members0[party=='CONG(I)',]$party=''

# Members1[party=='NCP',]$party=''
# Members1[party=='N.P.F.',]$party=''