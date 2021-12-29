rm(list=ls())
library(Hmisc)
install.packages("factoextra")
library(factoextra)
install.packages("dplyr")
library(dplyr)
data <- read.csv("C:/Users/ARSALAN IQBAL/Downloads/COVID19_line_list_data.csv")
data <- select(data,-c(X.1:X.6))
data <- select(data,-c(X))
describe(data)
data_copied <- data

data_copied$death_dummy <- as.integer(data$death !=0)
#data_deathdummy <- as.integer(data$death !=0)
#df <- sum(data$death_dummy) / nrow(data)
data_copied <- select(data_copied,-c(2,9,10,11,12,13,18,19))
data_copied <- na.omit(data_copied)



death_all = subset(data_copied,death_dummy == 1)
alive_all = subset(data_copied,death_dummy == 0)

men <- subset(data_copied, gender == "male")
women <- subset(data_copied, gender == "female")
mendeath <- subset(men,death_dummy == 1)
womendeath <- subset(women,death_dummy == 1)

#men_Age <- subset(men,age !=0)

menalive <- subset(men,death_dummy == 0)
womenalive <- subset(women,death_dummy == 0)

mean_mendeath <- mean(men$death_dummy)
mean_womendeath <- mean(women$death_dummy)

mean_age_death <- mean(death_all$age)
mean_age_alive <- mean(alive_all$age)

#death_female = subset(data_copied,gender == "female")
#death_male = subset(data_copied,gender == "male")

#deathage =(death_all$age)
#deathage_female = (death_female$age)
#deathage_male = (death_male$age)
#D <- factor(death_female$age)
#table(death_female$age)

A <- as.numeric(factor(death_female$age))
B <- as.numeric(table(death_female$age))

C <- as.character(factor(death_female$age))

barplot(B,main="FEMALES DEATH PER AGE",xlab="AGE",ylab="FREQUENCY")
barplot(death_male$age, main="MALES DEATH PER AGE")

t.test(alive_all$age, death_all$age, alternative="two.sided", conf.level = 0.95)

t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)



pcadata$age <- data_copied()











head(data_copied)
data_copiedpc <- data_copied[,c(7,8,9)]
pc.data <- princomp(data_copiedpc, cor = TRUE)
names(pc.data)
summary(pc.data)
eigenvectors <- pc.data$loadings
eigenvalues <- pc.data$sdev *pc.data$sdev
screeplot(pc.data,type="l", main="screeplot for the covid data")
abline(1,0,col= 'red',lty=2)

#REGRESSION

plot(data_copied$age,data_copied$death_dummy)

#CLUSTERING

data_ultra <- select(data_copied,-c(1,2,3,4,5,6,10,11,12))
data_ultra <- na.omit(data_ultra)
data_ultra <- inf.omit(data_ultra)



km <- kmeans(data_ultra, centers = 5, nstart = 100)
fviz_cluster(km, data = data_ultra)
