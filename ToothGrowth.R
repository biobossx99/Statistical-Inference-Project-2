##Coursera Statistical Inference - Allen Seol
##Class Project - ToothGrowth Data Set

##The response is the length of odontoblasts (teeth) in each of 10 guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) 
##with each of two delivery methods (orange juice or ascorbic acid).

##Load DataSet ToothGrowth and Using Dplyr Package
data(ToothGrowth) 
library(dplyr)
library(ggplot2)

#Change Doses to Factors
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

#subsetting by Dose
dose5 <- filter(ToothGrowth, dose == 0.5 )
dose1 <- filter(ToothGrowth, dose == 1.0 )
dose2 <- filter(ToothGrowth, dose == 2.0 )

#subsetting by Delivery
VC <- filter(ToothGrowth,supp =="VC")
OJ <- filter(ToothGrowth,supp =="OJ")


summary(ToothGrowth)

#boxplot OJ vs VC
boxplot(len~supp, data=ToothGrowth, col=(c("red","blue")), 
        main = "Growth by Delivery Method", xlab ="Delivery Method", ylab ="Length")

#boxplot by dose
boxplot(len~dose, data=ToothGrowth, col=(c("red","blue","green")), 
        main = "Growth by Difference Doses", xlab ="Dose", ylab ="Length")

#boxplot by dose and grouping
boxplot(len~supp*dose, data=ToothGrowth,col=(c("red","blue")),
        main = "Growth by Difference Doses and Delivery Method", xlab ="Dose & Delivery Method", ylab ="Length")

#subsetting by Dose
dose5 <- filter(ToothGrowth, dose == 0.5 )
dose1 <- filter(ToothGrowth, dose == 1.0 )
dose2 <- filter(ToothGrowth, dose == 2.0 )

#subsetting by Delivery
VC <- filter(ToothGrowth,supp =="VC")
OJ <- filter(ToothGrowth,supp =="OJ")

#T Test by delivery method
t.test(VC$len,OJ$len,paired = FALSE, var.equal = FALSE)

#T Test by Dose Comparison
## .5 and 1
t.test(dose5$len,dose1$len,paired = FALSE, var.equal = FALSE)

## .5 and 2
t.test(dose5$len,dose1$len,paired = FALSE, var.equal = FALSE)

## 1 and 2
t.test(dose1$len,dose2$len,paired = FALSE, var.equal = FALSE)
