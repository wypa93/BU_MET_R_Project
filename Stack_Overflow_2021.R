library(plotly) 
library(UsingR)
options(scipen=0)
data<-read.csv('/Users/wypa/Google Drive/Boston University /CS544_Fundamentals_of_R/Project/SO_Survey/survey_results_responses.csv')

#investigate and transform datatypes where required
colnames(us_data)
typeof(data$TotalComp)
data$YearsCode<-as.integer(data$YearsCode)
data$YearsCodePro<-as.integer(data$YearsCodePro)

View(us_data)

#Analysis of US Responses only 
table(data$Gender)
us_data<-subset(data,Country == 'United States of America'&
                Currency == 'USD\tUnited States dollar'&
                Gender == 'Man' | Gender == 'Woman')

plot_ly(us_data,x=~Ethnicity)

#search for and remove outliers using 1.5 IQR
f<-fivenum(us_data$CompTotal);f
subset(us_data, CompTotal > f[4] + 1.5*(f[4] - f[2])) #investigate upper outliers
subset(us_data, CompTotal < f[2] - 1.5*(f[4] - f[2])) #investigate lower outliers
us_data<-subset(us_data,CompTotal<f[4]+1.5*(f[4]-f[2])) #remove upper outliers
us_data<-subset(us_data,CompTotal>f[2]-1.5*(f[4]-f[2])) #remove lower outliers

plot_ly(us_data, x = ~CompTotal, type="box", name = 'Total Compensation',
        boxpoints = "all", jitter = 0.2, pointpos = -1.5)


#visualize distributions age, experience, gender, ethnicity,tech stack, degree
plot_ly(us_data, x = ~Gender)
us_f_salary<-subset(us_data,Gender = 'Woman')$ConvertedCompYearly
plot_ly(us_data,x=~CompTotal,color=~Gender,type='histogram')



#search for correlations between salary and age, experience, gender, ethnicity,tech stack, degree
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Gender')
plot_ly(us_data,x=~YearsCodePro,y=~CompTotal,type = 'scatter',color ='Years Code')


#identify most popular Tech-Stacks and display them as Word Cloud


#show US map with coders origin
