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

### PRE-PROCESSING ###
# investigate and transform datatypes where required ###
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




### ANALYSIS OF GENDER DIFFERENCES ###
female <- subset(us_data,Gender == 'Woman')
male <- subset(us_data,Gender == 'Man')
density1 <- density(female$CompTotal)
density2 <- density(male$CompTotal)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Total Compensation'),
                      yaxis = list(title = 'Density'));fig
## Analyze Years of Professional Code Experience by Gender
density3 <- density(drop_na(female,YearsCodePro)$YearsCodePro)
density4 <- density(drop_na(male,YearsCodePro)$YearsCodePro)

fig <- plot_ly(x = ~density3$x, y = ~density3$y, type = 'scatter', mode = 'lines', name = 'Women', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density4$x, y = ~density4$y, name = 'Man', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Years of Professional Coding Experience'),
                      yaxis = list(title = 'Density'));fig
## Analyze Education Level by Gender
ed_tab_m <- table(male$EdLevel)
ed_tab_m <- ed_tab_m / sum(as.numeric(ed_tab_m))
ed_tab_m <- sort(ed_tab_m,decreasing = TRUE)

ed_tab_f <- table(female$EdLevel)
ed_tab_f <- ed_tab_f / sum(as.numeric(ed_tab_f))
ed_tab_f <- sort(ed_tab_f,decreasing = TRUE)

fig <- plot_ly(type='bar',y=round(as.numeric(ed_tab_m),2),x=names(ed_tab_m),name='Men')
fig %>% add_trace(type='bar',y=round(as.numeric(ed_tab_f),2),x=names(ed_tab_f),name='Woman')


### Analyze Pay Gap by Ethnicity ###
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


## Central Limit Theorem ##
sample.sizes <- c(20,30,40,50)
sample.means <- c()
sample.dev <- c()
# Generate 1000 Samples of different sizes
getSamples <- function(size){
  samples <- 10000
  xbar <- numeric(samples)
  for (i in 1: samples) {
   xbar[i] <- mean(sample(us_data$CompTotal,size=size,replace=TRUE))
  }
  xbar
}
# Visualize outcome as Histograms
fig1 <- plot_ly(x = ~getSamples(10), type = "histogram",name='size 10')
fig2 <- plot_ly(x = ~getSamples(100), type = "histogram",name='size 100')
fig3 <- plot_ly(x = ~getSamples(200), type = "histogram",name='size 200')
fig4 <- plot_ly(x = ~getSamples(400), type = "histogram",name='size 400')
fig <- plotly:: subplot(fig1,fig2,fig3,fig4,nrows=2)%>%
  layout(title='Randomized Sampling of Total Compensation');fig

#print means and standard deviations for each size
for (i in sample.sizes){
  x<-getSamples(i)
  sample.means <- c(sample.means,mean(x))
  sample.dev <- c(sample.dev,sd(x))
}
sprintf('Sample Size: %i, Mean: %f, Standard Deviation, %f',sample.sizes,sample.means,sample.dev)



#identify most popular Tech-Stacks and display them as Word Cloud


#show US map with coders origin


# This is Ray's section






####

# This is Maksim's section





####
