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
sessions<- read.csv("sessions_RS.csv")
barplot(Members_Cleaned$party_normalized,xlab="Party",ylab="Numer of Member terms served")
