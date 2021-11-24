library(plotly)
library(UsingR)
library(usethis)
library(dplyr)
library(tidyr)
library(sampling)
library(sm)

options(scipen=0)
data<-read.csv('/Users/wypa/Google Drive/Boston University /CS544_Fundamentals_of_R/Project/SO_Survey/survey_results_responses.csv')
View(data)


### investigate and transform datatypes where required ###
colnames(us_data)
data$TotalComp<-as.intger(data$TotalComp)
data$YearsCode<-as.integer(data$YearsCode)
data$YearsCodePro<-as.integer(data$YearsCodePro)

#Analysis of US Responses only where Total Compensation > US Federal Minimum Wage 2021
us_data<-subset(data,Country == 'United States of America'&
                Currency == 'USD\tUnited States dollar'&
                  CompTotal > 15000 &
                (Gender == 'Man' | Gender == 'Woman'))


#search for and remove  outliers in Salary data using 1.5 IQR
f<-fivenum(us_data$CompTotal);f
subset(us_data, CompTotal > f[4] + 1.5*(f[4] - f[2])) 
us_data<-subset(us_data,CompTotal<f[4]+1.5*(f[4]-f[2])) 

### Analyze Pay Gap by Gender
female <- subset(us_data,Gender == 'Woman')
male <- subset(us_data,Gender == 'Man')
density1 <- density(female$CompTotal)
density2 <- density(male$CompTotal)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Total Compensation'),
                      yaxis = list(title = 'Density'))
fig

## Analyze Years of Professional Code Experience by Gender
density3 <- density(drop_na(female,YearsCodePro)$YearsCodePro)
density4 <- density(drop_na(male,YearsCodePro)$YearsCodePro)

fig <- plot_ly(x = ~density3$x, y = ~density3$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density4$x, y = ~density4$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Years of Professional Coding Experience'),
                      yaxis = list(title = 'Density'))
fig

## Analyze Education Level by Gender
ed_tab_m <- table(male$EdLevel)
ed_tab_m <- ed_tab_m / sum(as.numeric(ed_tab_m))
ed_tab_m <- sort(ed_tab_m,decreasing = TRUE)

ed_tab_f <- table(female$EdLevel)
ed_tab_f <- ed_tab_f / sum(as.numeric(ed_tab_f))
ed_tab_f <- sort(ed_tab_f,decreasing = TRUE)

fig <- plot_ly(type='bar',y=round(as.numeric(ed_tab_m),2),x=names(ed_tab_m),name='Men')
fig %>% add_trace(type='bar',y=round(as.numeric(ed_tab_f),2),x=names(ed_tab_f),name='Woman')


# Analyze Pay Gap by Ethnicity
ethn <- table(us_data$Ethnicity)
ethn<-ethn[ethn > 100]
names(ethn)

fig <- plot_ly(type = 'scatter', mode = 'line',fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density(subset(us_data,Ethnicity == 'Black or of African descent')$CompTotal)$x
                         , y = ~density(subset(us_data,Ethnicity == 'Black or of African descent')$CompTotal)$y
                         , name = 'Black or of African descent')
fig <- fig %>% add_trace(x = ~density(subset(us_data,Ethnicity == 'East Asian')$CompTotal)$x
                         , y = ~density(subset(us_data,Ethnicity == 'East Asian')$CompTotal)$y
                         , name = 'East Asian')
fig <- fig %>% add_trace(x = ~density(subset(us_data,Ethnicity == 'South Asian')$CompTotal)$x
                         , y = ~density(subset(us_data,Ethnicity == 'South Asian')$CompTotal)$y
                         , name = 'South Asian')
fig <- fig %>% add_trace(x = ~density(subset(us_data,Ethnicity == 'Hispanic or Latino/a/x')$CompTotal)$x
                         , y = ~density(subset(us_data,Ethnicity == 'Hispanic or Latino/a/x')$CompTotal)$y
                         , name = 'Hispanic or Latino/a/x')
fig <- fig %>% add_trace(x = ~density(subset(us_data,Ethnicity == 'White or of European descent')$CompTotal)$x
                         , y = ~density(subset(us_data,Ethnicity == 'White or of European descent')$CompTotal)$y
                         , name = 'White or of European descent')
fig <- fig %>% layout(xaxis = list(title = 'Total Compensation'),
                      yaxis = list(title = 'Density'))
fig


#search for correlations between salary and age, experience, gender, ethnicity,tech stack, degree
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Gender')
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Years Code')

# Central Limit Theorem
s <- srswr(100, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.1 <- us_data[rows, ]

s <- srswr(200, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.2 <- us_data[rows, ]

fig <- plot_ly(sample.1,x=~CompTotal)
fig %>% add_trace(sample.2,x=~CompTotal);fig


#identify most popular Tech-Stacks and display them as Word Cloud


#show US map with coders origin


# This is Ray's section






####

# This is Maksim's section





####
