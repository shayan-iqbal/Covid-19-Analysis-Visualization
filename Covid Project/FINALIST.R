rm(list=ls())
library(Hmisc)
library(factoextra)
library(dplyr)


data <- read.csv("C:/Users/ARSALAN IQBAL/Downloads/COVID19_line_list_data.csv")
data <- select(data,-c(X.1:X.6))
data <- select(data,-c(X))
describe(data)
summary(data)
data_copied <- data
data_copied$death_dummy <- as.integer(data$death !=0)

data_copied <- select(data_copied,-c(2,9,10,11,12,13,18,19))
data_copied <- na.omit(data_copied)

death_all = subset(data_copied,death_dummy == 1)
alive_all = subset(data_copied,death_dummy == 0)

hist(death_all$age)

men <- subset(data_copied, gender == "male")
women <- subset(data_copied, gender == "female")
mendeath <- subset(men,death_dummy == 1)
womendeath <- subset(women,death_dummy == 1)

menalive <- subset(men,death_dummy == 0)
womenalive <- subset(women,death_dummy == 0)

mean_mendeath <- mean(men$death_dummy)
mean_womendeath <- mean(women$death_dummy)

mean_age_death <- mean(death_all$age)
mean_age_alive <- mean(alive_all$age)

hist(mendeath$age, col='brown')
hist(womendeath$age, col='light blue')


t.test(alive_all$age, death_all$age, alternative="two.sided", conf.level = 0.95)

t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)

#Principle Component Analysis

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

km <- kmeans(data_ultra, centers = 5, nstart = 100)
fviz_cluster(km, data = data_ultra)
