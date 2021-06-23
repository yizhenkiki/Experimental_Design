install.packages("car")
require(car)

getwd()
#Clean data
mydata <- read.csv("Experimental_Design.csv")
View(mydata)

cleanData <- function(dataset){
  result <- ""
  for(i in 1:length(dataset)){
    if(dataset[i]=="非常不喜歡"){
      result[i] <- "1";
    }else if(dataset[i]=="稍微不喜歡"){
      result[i] <- "2";
    }else if(dataset[i]=="普通喜歡"){
      result[i] <- "3";
    }else if(dataset[i]=="稍微喜歡"){
      result[i] <- "4";
    }else{
      result[i] <- "5";
    }
  }
  return(result)
}

mydata$Q1 <- as.numeric(cleanData(mydata$Q1))
mydata$Q2 <- as.numeric(cleanData(mydata$Q2))
mydata$Q3 <- as.numeric(cleanData(mydata$Q3))
mydata$Q4 <- as.numeric(cleanData(mydata$Q4))
mydata
str(mydata)


require(car)
#Input data
#Question 1
productA <- mydata$Q1[1:5]
productB <- mydata$Q1[6:10]
productC <- mydata$Q1[11:15]
mydata2 <- data.frame(productA,productB,productC)
mydata2

#Sort out data
mydata3 <- stack(mydata2)
mydata3

#H0: For each of responses is the same
#H1: The distribution of responses is not the same
#Pre-test
#Homogeneous
attach(mydata3)
leveneTest(values~ind)

#H0: mu1 = mu2 = mu3
#H1: At least one mean differ
#ANOVA
aov1 <- aov(values~ind, data = mydata3)
summary(aov1)


#H0: The data are from a normal distribution 
#H1: The data are not normally distributed
#Normality
res <- aov1$residuals
qqnorm(res)
qqline(res)
shapiro.test(res)

#Post-test
TukeyHSD(aov1)

#Input data
#Question 2
productA <- mydata$Q2[1:5]
productB <- mydata$Q2[6:10]
productC <- mydata$Q2[11:15]
mydata2 <- data.frame(productA,productB,productC)
mydata2

#Sort out data
mydata3 <- stack(mydata2)
mydata3


#H0: For each of responses is the same
#H1: The distribution of responses is not the same
#Pre-test
#Homogeneous
attach(mydata3)
leveneTest(values~ind)


#H0: mu1 = mu2 = mu3
#H1: At least one mean differ
#ANOVA
aov1 <- aov(values~ind, data = mydata3)
summary(aov1)


#H0: The data are from a normal distribution 
#H1: The data are not normally distributed
#Normality
res <- aov1$residuals
qqnorm(res)
qqline(res)
shapiro.test(res)

#Post-test
TukeyHSD(aov1)

#Input data
#Question 3
productA <- mydata$Q3[1:5]
productB <- mydata$Q3[6:10]
productC <- mydata$Q3[11:15]
mydata2 <- data.frame(productA,productB,productC)
mydata2

#Sort out data
mydata3 <- stack(mydata2)
mydata3


#H0: For each of responses is the same
#H1: The distribution of responses is not the same
#Pre-test
#Homogeneous
attach(mydata3)
leveneTest(values~ind)


#H0: mu1 = mu2 = mu3
#H1: At least one mean differ
#ANOVA
aov1 <- aov(values~ind, data = mydata2)
summary(aov1)


#H0: The data are from a normal distribution 
#H1: The data are not normally distributed
#Normality
res <- aov1$residuals
qqnorm(res)
qqline(res)
shapiro.test(res)

#Post-test
TukeyHSD(aov1)

#Input data
#Question 4
productA <- mydata$Q4[1:5]
productB <- mydata$Q4[6:10]
productC <- mydata$Q4[11:15]
mydata2 <- data.frame(productA,productB,productC)
mydata2

#Sort out data
mydata3 <- stack(mydata2)
mydata3

#H0: For each of responses is the same
#H1: The distribution of responses is not the same
#Pre-test
#Homogeneous
attach(mydata3)
leveneTest(values~ind)


#H0: mu1 = mu2 = mu3
#H1: At least one mean differ
#ANOVA
aov1 <- aov(values~ind, data = mydata3)
summary(aov1)


#H0: The data are from a normal distribution 
#H1: The data are not normally distributed
#Normality
res <- aov1$residuals
qqnorm(res)
qqline(res)
shapiro.test(res)

#Post-test
TukeyHSD(aov1)



