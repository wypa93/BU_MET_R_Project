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
# Maksim's libraries
library(prob)
library(sampling)

options(scipen=0)
# Open from Pascal's PC
data<-read.csv('/Users/wypa/Google Drive/Boston University /CS544_Fundamentals_of_R/Project/SO_Survey/survey_results_responses.csv')
# Open from Ray's PC
data<-read.csv('/Users/rayhan/Documents/School/BU/1) Fall 2021/CS 544 (Foundations of Analytics with R)/Final Project/BU_MET_R_Project/survey_results_responses.csv')
# Open from Maxim's PC
data <- read.csv('/Users/maksimromancuk/Desktop/CS544 and R/CS544_project/BU_MET_R_Project/survey_results_responses.csv')

### PRE-PROCESSING ###
# investigate and transform datatypes where required ###
data$CompTotal<-as.integer(data$CompTotal)
data$YearsCode<-as.integer(data$YearsCode)
data$YearsCodePro<-as.integer(data$YearsCodePro)
#Remove Rows where Numeric Values are missing
data <- subset(data,is.na(data$CompTotal)==FALSE &
                 is.na(data$YearsCode)==FALSE &
                 (is.na(data$YearsCodePro)==FALSE))
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
#if stuck
getDonut(us_data$NEWStuck,'What to do when you are stuck?')
#Education
getDonut(us_data$EdLevel,'What is your highest education level?')
#Learn Code
getDonut(us_data$LearnCode,'How did you learn to code?')
#First Code Age
getDonut(us_data$Age1stCode,'At what age did you start coding for the first time?')
#Main Branch
getDonut(us_data$MainBranch,'What is your occupation?')
#Employment
getDonut(us_data$Employment,'Are you working for an employer or do you freelance?')
#DevType
getDonut(us_data$DevType,'What dev type are you?')
#SO visitor frequency
getDonut(us_data$SOVisitFreq,'How frequently do you visit Stack Overflow?')



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
ed_tab_m <- sort(ed_tab_m,decreasing = FALSE)
ed_tab_f <- table(female$EdLevel)
ed_tab_f <- ed_tab_f / sum(as.numeric(ed_tab_f))
ed_tab_f <- sort(ed_tab_f,decreasing = FALSE)
fig <- plot_ly(type='bar',x=round(as.numeric(ed_tab_m),2),y=names(ed_tab_m),name='Men')
fig %>% add_trace(type='bar',x=round(as.numeric(ed_tab_f),2),y=names(ed_tab_f),name='Woman')%>%
  layout( yaxis = list(title = 'Educational Gender Comparison',
                       categoryorder = "array",
                       categoryarray = ~as.numeric(ed_tab_f)),
          yaxis = list(title = "Frequency")
  )



### CENTRAL LIMIT THEOREM ###
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



# This is Ray's section
# Obtaining the map from the library
s <- map_data('state')

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

### new version with plotly
#plotly
Coders_Origin <- as.data.frame(table(us_data$US_State))

Coders_Origin$code <- state.abb[match(Coders_Origin$Var1,state.name)]

Coders_Origin$hover <- with(Coders_Origin, paste("Amount of coders", Freq))

#give states boundaries a white border
l <- list(color = toRGB("white"), width = 2)

#specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(Coders_Origin, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~Freq, text = ~hover, locations = ~code,
  color = ~Freq, colors = 'Purples'
)

fig <- fig %>% colorbar(title = "Amount of coders")
fig <- fig %>% layout (
  title = "Amount of coders by state (Hover for breakdown)",
  geo = g
)

fig


# This is Maksim's section
library(prob)
library(sampling)
fivenum(data$CompTotal)
# change NA in CompTotal column for mean values
data$ConvertedCompYearly
data$ConvertedCompYearly[-is.na(data$ConvertedCompYearly)]

library(tidyr)
no_na <- na.omit(data$ConvertedCompYearly)
data_1 <- data[data$ConvertedCompYearly %in% no_na, ]
#take data without outliers
min_out <- fivenum(data_1$ConvertedCompYearly)[2] - 1.5*(fivenum(data_1$ConvertedCompYearly)[4]-fivenum(data_1$ConvertedCompYearly)[2])
max_out <- fivenum(data_1$ConvertedCompYearly)[4] + 1.5*(fivenum(data_1$ConvertedCompYearly)[4]-fivenum(data_1$ConvertedCompYearly)[2])
data_1_no_out <- data_1[(data_1$ConvertedCompYearly> min_out) & (data_1$ConvertedCompYearly< max_out), ]
min_val = floor(min(data_1_no_out$ConvertedCompYearly))
max_val = ceiling(max(data_1_no_out$ConvertedCompYearly))
step = (max_val-min_val)/20
hist(data_1_no_out$ConvertedCompYearly, breaks = c(seq(from = min_val, to = max_val, by = step)))
fivenum(data_1_no_out$ConvertedCompYearly)
boxplot()


#MAXIM SAMPLING HOPE FINAL
library(plotly)
#data cleaning
us_data_1 <- us_data[us_data$ConvertedCompYearly %in% na.omit(us_data$ConvertedCompYearly) , ]
ten_perc <- nrow(us_data_1)*0.1
min_out <- fivenum(us_data_1$ConvertedCompYearly)[2] - 1.5*(fivenum(us_data_1$ConvertedCompYearly)[4]-fivenum(us_data_1$ConvertedCompYearly)[2])
max_out <- fivenum(us_data_1$ConvertedCompYearly)[4] + 1.5*(fivenum(us_data_1$ConvertedCompYearly)[4]-fivenum(us_data_1$ConvertedCompYearly)[2])
us_data_1 <- us_data_1[(us_data_1$ConvertedCompYearly> min_out) & (us_data_1$ConvertedCompYearly< max_out), ]
ten_perc <- nrow(us_data_1)*0.1

#plots of population
fig1<-plot_ly(us_data_1,x=~ConvertedCompYearly, type = "histogram",name='The population dataset', nbinsx = 20)%>%
  layout(xaxis= list(showticklabels = FALSE)); fig1

fig1_bx <- plot_ly(us_data_1,x=~ConvertedCompYearly, type = "box",name='The population dataset')%>%
  layout(xaxis= list(showticklabels = FALSE)); fig1_bx


#sampling -- srs without replacement
#take the most popular states
top_states <- sort(table(us_data_1$US_State),decreasing=TRUE)[1:5]
#taking the subset of these states
states_srs <- subset(us_data_1,us_data_1$US_State %in% names(top_states))
set.seed(9999)
size = ten_perc
#randomly chooses rows for further analysis
s<-srswor(size, nrow(states_srs))
states_srs <- states_srs[s != 0, ]
fig2<-plot_ly(states_srs,x=~ConvertedCompYearly, type = "histogram",name='SRS without replacement', nbinsx = 20)%>%
  layout(xaxis= list(showticklabels = FALSE)); fig2
fig2_bx <- plot_ly(states_srs,x=~ConvertedCompYearly, type = "box",name='SRS without replacement')%>%
  layout(xaxis= list(showticklabels = FALSE)); fig2_bx


#making the subset of interested states for further convenience
subset_states <- subset(us_data_1,us_data_1$US_State %in% names(top_states))

# -- systematic sampling
k <- ceiling(nrow(subset_states)/size) #N rows are divided into n = size groups and each group has k items
r<-sample(k, 1)#random item from k is selected
indexes = seq(r, by = k, length = size) #all items are selected by taking every k-th item from the frame
subset_systematic <- subset_states[indexes, ]
fig3<-plot_ly(subset_systematic,x=~ConvertedCompYearly, type = "histogram",name='Systematic Sampling', nbinsx = 20)%>%
  layout(xaxis= list(showticklabels = FALSE)); fig3
fig3_bx <- plot_ly(subset_systematic,x=~ConvertedCompYearly, type = "box",name='Systematic Sampling')%>%
  layout(xaxis= list(showticklabels = FALSE)); fig3_bx


#-- inclusion probabilities
pik<-inclusionprobabilities(subset_states$ConvertedCompYearly,size)
sum(pik)
s<-UPsystematic(pik)
sample<-subset_states[s!=0,]
fig4<-plot_ly(sample, x=~ConvertedCompYearly, type = "histogram",name='Inclusion probabilities', nbinsx = 20); fig4
fig4_bx <- plot_ly(sample,x=~ConvertedCompYearly, type = "box",name='Inclusion probabilities')%>%
  layout(xaxis= list(showticklabels = FALSE)); fig4_bx


#--stratified sampling based on the Country variable
subset_states<-subset_states[order(subset_states$US_State),]
size_st<-table(subset_states$US_State)/sum(table(subset_states$US_State))*size

st.1 <- sampling::strata(subset_states, stratanames = c("US_State"),
                         size = size_st, method = "srswor",
                         description = TRUE)
st.sample1 <- getdata(subset_states, st.1)
fig5<-plot_ly(st.sample1, x=~ConvertedCompYearly, type = "histogram",name='Stratified Sampling', nbinsx = 20)%>%
  layout(xaxis= list(showticklabels = FALSE)); fig5
fig5_bx <- plot_ly(st.sample1,x=~ConvertedCompYearly, type = "box",name='Stratified Sampling')%>%
  layout(xaxis= list(showticklabels = FALSE)); fig5_bx
fig <- plotly:: subplot(fig1,fig2,fig3,fig4, fig5, nrows =5)%>%
  layout(showlegend = FALSE)
fig

fig_bx <- plotly:: subplot(fig1_bx,fig2_bx,fig3_bx,fig4_bx, fig5_bx, nrows =5)%>%
  layout(showlegend = FALSE)
fig_bx


