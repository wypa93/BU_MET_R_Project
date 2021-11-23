library(plotly) 
library(UsingR)
library(usethis)
library(dplyr)
library(tidyr)
library(sm)

options(scipen=0)
data<-read.csv('/Users/wypa/Google Drive/Boston University /CS544_Fundamentals_of_R/Project/SO_Survey/survey_results_responses.csv')

#investigate and transform datatypes where required
colnames(us_data)
typeof(data$TotalComp)
data$YearsCode<-as.integer(data$YearsCode)
data$YearsCodePro<-as.integer(data$YearsCodePro)

View(us_data)

#Analysis of US Responses only where Total Compensation > US Federal Minimum Wage 2021
us_data<-subset(data,Country == 'United States of America'&
                Currency == 'USD\tUnited States dollar'&
                  CompTotal > 15000 &
                (Gender == 'Man' | Gender == 'Woman'))


#search for and remove  outliers in Salary data using 1.5 IQR
f<-fivenum(us_data$CompTotal);f
subset(us_data, CompTotal > f[4] + 1.5*(f[4] - f[2])) #investigate upper outliers
us_data<-subset(us_data,CompTotal<f[4]+1.5*(f[4]-f[2])) #remove upper outliers

plot_ly(us_data, x = ~CompTotal, type="box", name = 'Total Compensation',
        boxpoints = "all", jitter = 0.2, pointpos = -1.5)


### Analyze Gender Pay Gap
female <- subset(us_data,Gender == 'Woman')
male <- subset(us_data,Gender == 'Man')
density1 <- density(female$CompTotal)
density2 <- density(male$CompTotal)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Total Compensation'),
                      yaxis = list(title = 'Density'))

fig

## Analyze Years of Code by Gender
density3 <- density(drop_na(female,YearsCodePro)$YearsCodePro)
density4 <- density(drop_na(male,YearsCodePro)$YearsCodePro)

fig <- plot_ly(x = ~density3$x, y = ~density3$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density4$x, y = ~density4$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Total Compensation'),
                      yaxis = list(title = 'Density'))

fig



# Analyze Ethnicity Pay Gap
ethn <- table(us_data$Ethnicity)
ethn[ethn > 100]

plot_ly(ethn)

white <- density(subset(us_data,Ethnicity == 'White or of European descent')$CompTotal)





s#search for correlations between salary and age, experience, gender, ethnicity,tech stack, degree
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Gender')
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Years Code')
#identify most popular Tech-Stacks and display them as Word Cloud


#show US map with coders origin

# This is Ray's section






####

# This is Maksim's section





####
