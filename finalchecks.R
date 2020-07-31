termstarts<- sort(allmembers$term_start)
termends <- sort(allmembers$term_end)
vacation_date <- sort(allmembers$vacation_date)
missingdates<- allmembers[is.na(allmembers$vacation_date),]
missingreasons<- allmembers[allmembers$vacation_reason == '',]

empty<- subset(Members7, party_normalized == '')
print(unique(empty$party))
