library(plotly)
library(UsingR)
library(usethis)
library(dplyr)
library(tidyr)
library(sampling)
library(sm)
library(stringr)

# Ray's libraries
library(maps)
library(mapproj)
library(mapdata)
library(ggplot2)

options(scipen=0)

# Open from Pascal's PC
data<-read.csv('/Users/wypa/Google Drive/Boston University /CS544_Fundamentals_of_R/Project/SO_Survey/survey_results_responses.csv')

# Open from Ray's PC
data<-read.csv('/Users/rayhan/Documents/School/BU/1) Fall 2021/CS 544 (Foundations of Analytics with R)/Final Project/BU_MET_R_Project/survey_results_responses.csv')

View(data)

### PRE-PROCESSING ###
# investigate and transform datatypes where required ###
colnames(us_data) #us_data not defined before this (Ray)
data$TotalComp<-as.integer(data$TotalComp) #typo here previously as.intger (Ray)
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


### CATEGORICAL DATA ###
#POPULAR TECH-STACKS
getPopularity <- function(workedWith,wantToWorkWith,topic){
  lang1 <- sort(table(unlist(strsplit(workedWith, split = ';',fixed = TRUE))),decreasing = FALSE)
  lang2 <- sort(table(unlist(strsplit(wantToWorkWith, split = ';',fixed = TRUE))),decreasing = FALSE)
  fig <- plot_ly(x=as.numeric(lang1),y=names(lang1),type = "bar", name = 'worked with') %>%
    add_trace(x=as.numeric(lang2),y=names(lang2),type = "bar",name = ' want to work with')%>%
    layout( yaxis = list(title = topic,
                        categoryorder = "array",
                        categoryarray = ~as.numeric(lang1)),
           yaxis = list(title = "Frequency")
    )
  fig
}
#Coding Languages
getPopularity(us_data$LanguageHaveWorkedWith,us_data$LanguageWantToWorkWith,'Languages')
#Databases
getPopularity(us_data$DatabaseHaveWorkedWith,us_data$DatabaseWantToWorkWith,'Databases')
#Coding Tools
getPopularity(us_data$ToolsTechHaveWorkedWith,us_data$ToolsTechWantToWorkWith,'Tools')
#Webframes
getPopularity(us_data$WebframeHaveWorkedWith,us_data$WebframeWantToWorkWith,'Webframes')
#Misc
getPopularity(us_data$MiscTechHaveWorkedWith,us_data$MiscTechWantToWorkWith,'Misc')
#Platforms
getPopularity(us_data$PlatformHaveWorkedWith,us_data$PlatformWantToWorkWith,'Webframes')
#Collab Tools
getPopularity(us_data$NEWCollabToolsHaveWorkedWith,us_data$NEWCollabToolsWantToWorkWith,'Collab Tools')

#DONUT CHARTS
getDonut <- function(values,name){
  vls <- sort(table(unlist(strsplit(values,split = ';',fixed = TRUE))),decreasing = FALSE)
  fig<-plot_ly(labels=names(vls),values=as.numeric(vls))
  fig <- fig %>% add_pie(hole = 0.6)
  fig <- fig %>% layout(title = name,  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
}
getDonut(us_data$NEWStuck,'What to do when you are stuck?')
getDonut(us_data$EdLevel,'What is your highest education level?')
getDonut(us_data$LearnCode,'How did you learn to code?')
getDonut(us_data$Age1stCode,'At what age did you start coding for the first time?')
getDonut(us_data$MainBranch,'What is your occupation?')
getDonut(us_data$Employment,'Are you working for an employer or do you freelance?')
getDonut(us_data$DevType,'What dev type are you?')


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


## Central Limit Theorem ##
sample.sizes <- c(20,30,40,50)
sample.means <- c()
sample.dev <- c()
set.seed(9066)
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
fig1 <- plot_ly(x = ~getSamples(20), type = "histogram",name='size 20',histnorm='density')%>%
  layout(yaxis=list(range=c(0,0.7)),xaxis=list(range=c(90000,180000)))
fig2 <- plot_ly(x = ~getSamples(30), type = "histogram",name='size 30',histnorm='density')%>%
  layout(yaxis=list(range=c(0,0.7)),xaxis=list(range=c(90000,180000)))
fig3 <- plot_ly(x = ~getSamples(40), type = "histogram",name='size 40',histnorm='density')%>%
  layout(yaxis=list(range=c(0,0.7)),xaxis=list(range=c(90000,180000)))
fig4 <- plot_ly(x = ~getSamples(50), type = "histogram",name='size 50',histnorm='density')%>%
  layout(yaxis=list(range=c(0,0.7)),xaxis=list(range=c(90000,180000)))
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
# US States
s <- map_data('state')
ggplot(s, aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(color = 'black') +
  coord_map('polyconic') +
  guides(fill = F)

# Coders from each state
Coders_Origin <- us_data %>% group_by(US_State) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Merge data
Coders_Origin$US_State <- tolower(Coders_Origin$US_State)
Coders_data <- merge(s, Coders_Origin,
                     by.x = 'region',
                     by.y = 'US_State')

# Plot
ggplot(Coders_data, aes(x = long, y = lat,
                        group = group,
                        fill = count)) +
  geom_polygon(color = 'gray') +
  coord_map('polyconic') +
  scale_fill_gradient2(low = 'white', high = 'red') +
  theme_void() +
  ggtitle('Amount of coders by State')
####

# This is Maksim's section





####
