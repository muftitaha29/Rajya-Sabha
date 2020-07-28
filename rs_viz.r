setwd('~/Documents/GitHub/Rajya-Sabha/')
library(data.table)
library(plotly)

Members_Cleaned<-fread('rs_cleaned.csv')
Members_Cleaned_Blank = Members_Cleaned[party_normalized=='' & party!='NOM.' & party!='O'
                                        & party!='CONG(I)' & party!='CONG (S)' & party!='CONG(O)'&
                                          party!='INC' & party!='Congress',]

fwrite(Members_Cleaned_Blank,"rs_clean_blanks.csv")


M3 <- Members_Cleaned[,if(.N == 3) .SD,by=name_of_member]
M4 <- Members_Cleaned[,if(.N == 4) .SD,by=name_of_member]
M5 <- Members_Cleaned[,if(.N == 5) .SD,by=name_of_member]
M6 <- Members_Cleaned[,if(.N == 6) .SD,by=name_of_member]


ggplot(M6, aes(M6$term_start, M6$name_of_member, color = M6$party_normalized, group=Item)) +
  geom_line(size = 10) +
  labs(x="Year", y=NULL, title="Terms Served")


# Gantt Charts
# Convert to dates
df$Start <- as.Date(df$Start, format = "%m/%d/%Y")

# Sample client name
client = "Sample Client"

# Choose colors based on number of resources
cols <- RColorBrewer::brewer.pal(length(unique(df$Resource)), name = "Set3")
df$color <- factor(df$Resource, labels = cols)

# Initialize empty plot
fig <- plot_ly()

# Each task is a separate trace
# Each trace is essentially a thick line plot
# x-axis ticks are dates and handled automatically

for(i in 1:(nrow(M6) - 1)){
  fig <- add_trace(fig,
                   x = c(M6$term_start[i], M6$term_start[i] + M6$term_end[i]),  # x0, x1
                   y = name_of_member(i),
                   mode = "lines",
                   line = list(color = df$color[i], width = 20),
                   showlegend = F,
                   
                   
                   evaluate = T  # needed to avoid lazy loading
  )
}

fig
#https://rpubs.com/sgetalbo/gantt_draft
#https://plotly.com/r/getting-started/
#https://plotly.com/r/plotly-fundamentals/
#https://plotly.com/r/gantt/

y<- Members_Cleaned[,.N,by=party_normalized]
barplot(Members_Cleaned$party_normalized,xlab="Party",ylab="Numer of Member terms served",)
barplot(y$N,xlab="Party",ylab="Numer of Member terms served")
y<- table(y)
barplot(y,xlab="Party",ylab="Numer of Member terms served",col="Members_Cleaned$party_normalized",)
sessions<- read.csv("sessions_rs.csv")
barplot(Members_Cleaned$party_normalized,xlab="Party",ylab="Numer of Member terms served")


nominated<- subset(Members_Cleaned, party_normalized=='NOMINATED')
nominated<- data.table(nominated)
years<- seq(as.Date("1952-01-01"), as.Date("2020-01-01"), by="years")
years<- data.table(years)
nominated$yearlist= ''
for (i in 1:nrow(nominated)){
  year_list = ''
  for (j in 1: nrow(years)){
    if (years$years[j] > nominated$term_start[i] && years$years[j] < nominated$vacation_date[i])
      {
      year_list = paste0(year_list,",",format(years$years[j], "%Y"))
      }
  }
  nominated$yearlist[i] = year_list
}






#### #CODE FOR ADDING YEAR DATA IN PYTHON #### 

# list_of_years = list(range(1947,2020))
# 
# def convert_year_to_datetime(year):
#   return pd.to_datetime('01-01-'+ str(year), format = '%d-%m-%Y')

# list_of_years_datetime = [convert_year_to_datetime(x) for x in list_of_years]

# year_list = []

# for i in range(0, len(cabinet_csv_file)):
#   year_string = ''
#   start_year = pd.to_datetime(cabinet_csv_file['appointment_begin_in_datetime'].iloc[i])
#   end_year = pd.to_datetime(cabinet_csv_file['appointment_end_in_datetime'].iloc[i])
#   for year in list_of_years_datetime:
#     if year >= start_year and year<= end_year:
#     year_string += year.strftime('%Y,')
#   year_list.append(year_string)
# 
# # assigning list to a new column in the pandas dataframe
# cabinet_csv_file['list_of_years'] = pd.Series(year_list).values
# print(cabinet_csv_file['list_of_years']) 
