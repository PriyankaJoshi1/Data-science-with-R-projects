getwd()
#Loading dataset into R
insdata<-read.csv('SwedishMotorInsurance.csv')
head(insdata)

#Question 1
#The committee is interested to know each field of the data collected through
#descriptive analysis to gain basic insights into the data set and to prepare 
#for further analysis
summary(insdata)
#we can see that the minimum claims and payments are 0 so there are some datapoints 
#with zero claims

#Question 2
#The total value of payment by an insurance company is an important factor to be 
#monitored. So the committee has decided to find whether this payment is related to 
#number of claims and the number of insured policy years. They also want to visualize 
#the results for better understanding.
cor(insdata$Claims,insdata$Payment)
#99% correlation means claims are highly correlated with payment
cor(insdata$Insured,insdata$Payment)
#93% correlation indicated that insured policy years is highly correlated to payment
# for better visualization
plot(insdata$Claims,insdata$Payment)
plot(insdata$Insured,insdata$Payment)
#from the plots we can see that with increase in claims and insured years payment also increases.

#Question 3
#The committee wants to figure out the reasons for insurance payment increase and decrease. 
#So they have decided to find whether distance, location, bonus, make, and insured amount or 
#claims are affecting the payment or all or some of these are affecting it.
#payment is the dependent variable
#Rest others are independent variable
linearmodel<-lm(Payment ~.,data = insdata)
summary(linearmodel)
#from the output we can infer that except bonus and make all other variables are significant variables 
#and has impact on the variable payment as they have less p-value

#Question 4
#The insurance company is planning to establish a new branch office, so they are interested to find at 
#what location, kilometer, and bonus level their insured amount, claims, and payment get increased. 
#(Hint: Aggregate Dataset)
unique(insdata$Kilometres)
unique(insdata$Bonus)
unique(insdata$Zone)
zonegroup<-apply(insdata[,c(5,6,7)],2,function(x) tapply(x,insdata$Zone,mean))
zonegroup
distgroup<-apply(insdata[,c(5,6,7)],2,function(x) tapply(x,insdata$Kilometres,mean))
distgroup
bonusgroup<-apply(insdata[,c(5,6,7)],2,function(x) tapply(x,insdata$Bonus,mean))
bonusgroup
#from the results we can infer that
#Zone 4 has the highest claims and payments and zone 7 has the lowest claims and payments also 
#most of the payments and claims come from zone 1 to 4
#kilometer group 5 has the lowest payment and group 2 has maximum payment
#bonus group 7 has drastically high insured,claims and payment compared to other groups

#Question 5
#The committee wants to understand what affects their claim rates so as to decide the right
#premiums for a certain set of situations. Hence, they need to find whether the insured 
#amount, zone, kilometer, bonus, or make affects the claim rates and to what extent.
#Claims is the dependent variable in this case and insured,kilometers,zone,bonus,make are independent
linearmodel2<-lm(Claims~Insured+Zone+Kilometres+Bonus+Make,data = insdata)
summary(linearmodel2)
# from the results and looking at the p-values we can infer that all the independent variables
#have significant impact on the claims 