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
s <- srswr(800, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.1 <- us_data[rows, ]
m1<-mean(sample.1$CompTotal)
sd1<-sd(sample.1$CompTotal)

s <- srswr(1200, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.2 <- us_data[rows, ]
m2<-mean(sample.2$CompTotal)
sd2<-sd(sample.2$CompTotal)

s <- srswr(1600, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.3 <- us_data[rows, ]
m3<-mean(sample.3$CompTotal)
sd3<-sd(sample.3$CompTotal)

s <- srswr(2500, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.4 <- us_data[rows, ]
m4<-mean(sample.4$CompTotal)
sd4<-sd(sample.4$CompTotal)

s <- srswr(3000, nrow(us_data))
rows <- (1:nrow(us_data))[s!=0]
rows <- rep(rows, s[s != 0])
sample.5 <- us_data[rows, ]
m5<-mean(sample.5$CompTotal)
sd5<-sd(sample.5$CompTotal)

m6<-mean(us_data$CompTotal)
sd6<-sd(us_data$CompTotal)

fig1 <- plot_ly(sample.1,x=~CompTotal, type = "histogram",name='size= 800')
fig2 <- plot_ly(sample.2,x=~CompTotal, type = "histogram",name='size= 1200')
fig3 <- plot_ly(sample.3,x=~CompTotal,type = "histogram",name='size= 1600')
fig4 <- plot_ly(sample.4,x=~CompTotal, type = "histogram",name='size= 2500')
fig5 <- plot_ly(sample.5,x=~CompTotal, type = "histogram",name='size= 3000')
fig6 <- plot_ly(us_data,x=~CompTotal, type = "histogram",name='Full data')
fig <- plotly:: subplot(fig1,fig2,fig3,fig4,fig5,fig6,nrows=3)%>%
  layout(title='Randomized Sampling of Total Compensation');fig

sampleSizes <- c(800,1200,1600,2500,3000)
means<-c(m1,m2,m3,m4,m5,m6)
deviations <-c(sd1,sd2,sd3,sd4,sd5,sd6)
sprint

#identify most popular Tech-Stacks and display them as Word Cloud


#show US map with coders origin


# This is Ray's section






####

# This is Maksim's section





####
