nosim<-1000
n<-40
lambda<-0.2
set.seed(123)

#Theoretical mean of the distribution

theoretical_mean<-1/lambda

theoretical_mean

#Mean of the Sample Means
mean(apply(matrix(rexp(n=nosim*n,rate=0.2),nosim),1,mean))

library(ggplot2)
library(reshape2)

means<- NULL
for (i in 1 :nosim) means = c(means, mean(rexp(n, lambda)))

sample_and_theoretical_means<-data.frame(sample=means,theoretical=c(rep(1/lambda,1000)))

means_grouped<-data.frame(sample=mean(means),theoretical=c(5))

means_type<-melt(means_grouped,variable.name="means.type",value.name = "value")

ggplot(sample_and_theoretical_means,aes(x=sample))+geom_histogram(aes(y=..count..),binwidth=0.2,colour="dark orange",fill="bisque")+
  labs(x="Sample Means",y="Frequency",title="Histogram of the Sample Means")+
  geom_vline(data=means_type,aes(xintercept = value, colour = means.type), size = 2, linetype = "dashed")
 


ggplot(data=as.data.frame(means),aes(x=means))+geom_histogram(aes(y=..density..),binwidth=0.2,colour="darkgreen",fill="white")+
  scale_x_continuous(breaks=2:8)+stat_function(fun=dnorm,args=list(mean=mean(means,na.rm = TRUE),sd=sd(means,na.rm=TRUE)),colour="red",
                                               size=1)+labs(x="Sample Means",y="Density",
                                                            title="Simulation of Exponentially Distributed Sample Means")+
  geom_density(colour="dark orange")

#Part 2


library(datasets)
library(ggplot2)

data(ToothGrowth)
str(ToothGrowth)

#Data Description
#The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC).
#The dose should also be a factor in order to implement t-test.


ToothGrowth$dose <- factor(ToothGrowth$dose)

ggplot(data=ToothGrowth, aes(x=supp, y=len)) + geom_boxplot(aes(fill=supp)) +
  labs(title = "Boxplot of ToothGrowth According to Supplement ",
       x = "Supplement Type", y = "Tooth length")


ggplot(data=ToothGrowth, aes(x=dose, y=len)) + geom_boxplot(aes(fill=dose)) +
  labs(title = "Boxplot of ToothGrowth According to Dose",
       x = "Dose in miligrams/day", y = "Tooth length")



summary(ToothGrowth)

t.test(len~supp,data=ToothGrowth)


ToothGrowth_v2<-subset(ToothGrowth,dose%in%c(0.5,1))

t.test(len~dose,paired=FALSE,var.equal=TRUE,data=ToothGrowth_v2)

ToothGrowth_v3<-subset(ToothGrowth,dose%in%c(1,2))

t.test(len~dose,paired=FALSE,var.equal=TRUE,data=ToothGrowth_v3)


