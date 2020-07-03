# this script compares the aggregates for each letter 
# of the scraped data with the actual aggregates that have been manually compiled

library(data.table)

setwd('~/Documents/GitHub/Rajya-Sabha/')
# read raw data file 
membersnew<- fread("all_rs_members_scraped.csv")

# get a new column with the first letters 
membersnew$first_letter = substr(membersnew$name_of_member,1,1)

# aggregate membersnew by first letter 
# derive the count 
# most frequently used - https://tcpd.ashoka.edu.in/wp-content/uploads/2019/09/week5-R.pdf 
membersnew_unique <- distinct(membersnew, name_of_member, .keep_all = T)
count<- membersnew_unique[, .(number_of_entries= .N), by=.(first_letter)]

# manually derived actual data aggregates 
actual_count = c(133,150,69,122,1,6,89,36,19,70,167,37,231,71,5,154,4,197,341,75,5,65,7,0,24,8)
corresponding_letter = c(LETTERS)

# create data table 
actual_agg = data.table()
actual_agg$letter = corresponding_letter
actual_agg$actual_count = actual_count

# merging the actual count and the computed count 
merge_actual_and_computed = merge(actual_agg, count, by.x = "letter", by.y ="first_letter", all.x=TRUE)
merge_actual_and_computed$difference = merge_actual_and_computed$actual_count - merge_actual_and_computed$number_of_entries

# it seems the difference is only because of the duplicate names for the letters 'A','B', 'M','S' 

# is it possible to compute unique instances of members? 
# we can use the 'no' column in conjunction with the 'name_of_member' to produce such an instance

membersnew$name_key = paste0(membersnew$no,membersnew$name_of_member)
membersnew_distinct_new <- distinct(membersnew, name_key, .keep_all = T)
count_new<- membersnew_distinct_new[, .(number_of_entries_new= .N), by=.(first_letter)]

# now merging this back to the original file 

merge_actual_and_computed = merge(merge_actual_and_computed, count_new, by.x = 'letter',
                                  by.y = "first_letter",all.x = TRUE)

merge_actual_and_computed$difference_new = merge_actual_and_computed$actual_count - merge_actual_and_computed$number_of_entries_new

# great! now we have no errors 
