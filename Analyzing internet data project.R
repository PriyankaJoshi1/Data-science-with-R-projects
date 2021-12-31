getwd()
#importing the dataset
webdata<-read.csv('Internet_Dataset.csv',header = TRUE)

#question1
#The team wants to analyze each variable of the data collected through data
#summarization to get a basic understanding of the dataset and to prepare
#for further analysis.
str(webdata)
summary(webdata)
webdata$Continent=as.factor(webdata$Continent)#converted to factor so that we get full summary
webdata$Sourcegroup=as.factor(webdata$Sourcegroup)
summary(webdata)
#looking at the summary results we can see that
#maximum bounces for the site is 30
#maximum exits are 36
#site was visited through Google maximum times
#it was accessed maximum times by visitors from N.America

#question 2
#As mentioned earlier, a unique page view represents the number of
#sessions during which that page was viewed one or more times. A visit
#counts all instances, no matter how many times the same visitor may have
#been to your site. So the team needs to know whether the unique page
#view value depends on visits

corr<-cor(webdata$Uniquepageviews,webdata$Visits,method = "pearson")
corr

ano<-aov(Uniquepageviews~Visits,data = webdata)
ano
summary(ano)
#we can infer from the results that no of visits have the significant impact on the Unique page views and 
#its value depends on the visits

#question 3
#Find out the probable factors from the dataset, which could affect the exits.
#Exit Page Analysis is usually required to get an idea about why a user leaves
#the website for a session and moves on to another one. Please keep in
#mind that exits should not be confused with bounces.

ano2<-aov(Exits~ .,data = webdata)
summary(ano2)
#from the results we can infer that bounces,sourcegroup,uniquepage views have the significant impact on the 
#exits and Visits does not have that much impact on exits

#question 4
#Every site wants to increase the time on page for a visitor. This increases
#the chances of the visitor understanding the site content better and hence
#there are more chances of a transaction taking place. Find the variables
#which possibly have an effect on the time on page

ano3<-aov(Timeinpage~ .,data = webdata)
ano3
summary(ano3)

#we can infer that except for the source group all the variables have the signoficant impact on the time on page 
#spent by the visitor

#question 5
#A high bounce rate is a cause of alarm for websites which depend on visitor
#engagement. Help the team in determining the factors that are impacting
#the bounce.

model<-glm(Bounces~ .,data = webdata)
model
summary(model)

#we can infer from the results that Visits,BouncesNew,Exits and Source group have significant impact on the bounce
