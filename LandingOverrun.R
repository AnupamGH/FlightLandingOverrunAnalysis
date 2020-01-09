#read csv files
faa1 <- read.csv('FAA1.csv')
faa2 <- read.csv('FAA2.csv')
library('plyr')
faa <- rbind.fill(faa1,faa2)

#remove duplicate rows from dataframe
library(tidyverse)
faa <- as_tibble(faa)
faa.no.dup <- distinct(faa,aircraft,no_pasg,speed_ground,speed_air,height,pitch,distance,.keep_all = T)
faa.no.dup <- faa.no.dup[1:850,]

#print missing values of each column 
sapply(faa.no.dup, function(x) sum(is.na(x)))

#correlation between the variables
faa.temp <- faa.no.dup[,-1]
corr <- cor(faa.temp$distance,faa.temp[,-7],use = "pairwise.complete.obs")
corr

#delete rows with missing value of duration column
removerows <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
faa.no.dup <- removerows(faa.no.dup, "duration")

faa.temp1 <- faa.no.dup[,-1]
corr1 <- cor(faa.temp1$distance,faa.temp1[,-7],use = "pairwise.complete.obs")
corr1

#output after missing values of duration are removed
sapply(faa.no.dup, function(x) sum(is.na(x)))

#checking correlation of speed_air variable with other variables 
corr2 <- cor(faa.no.dup$speed_air,faa.temp1[,-4],use = "pairwise.complete.obs")
corr2 

#check abnormal values

height_abnormal <- sum(faa.no.dup$height<6)
duration_abnormal <- sum(faa.no.dup$duration<40)
speed_ground_abnormal <- sum(faa.no.dup$speed_ground<30 | faa.no.dup$speed_ground > 140)
height_abnormal;duration_abnormal;speed_ground_abnormal

#remove abnormal values
faa.no.dup <- faa.no.dup[faa.no.dup$height > 6,]
faa.no.dup <- faa.no.dup[faa.no.dup$duration > 40,]
faa.no.dup <- faa.no.dup[faa.no.dup$speed_ground > 30,]
faa.no.dup <- faa.no.dup[faa.no.dup$speed_ground < 140,]

#summary of variables
summary(faa.no.dup$duration)
summary(faa.no.dup$no_pasg)
summary(faa.no.dup$speed_ground)
summary(faa.no.dup$speed_air)
summary(faa.no.dup$height)
summary(faa.no.dup$pitch)
summary(faa.no.dup$distance)

#plot scatterplots between response(distance) and predictor variables
plot(faa.no.dup$duration,faa.no.dup$distance, xlab = 'duration', ylab = 'distance')
plot(faa.no.dup$no_pasg,faa.no.dup$distance, xlab = 'no_pasg', ylab = 'distance')
plot(faa.no.dup$speed_ground,faa.no.dup$distance, xlab = 'speed_ground', ylab = 'distance')
plot(faa.no.dup$speed_air,faa.no.dup$distance, xlab = 'speed_air', ylab = 'distance')
plot(faa.no.dup$height,faa.no.dup$distance, xlab = 'height', ylab = 'distance')
plot(faa.no.dup$pitch,faa.no.dup$distance, xlab = 'pitch', ylab = 'distance')

faa.temp2 <- faa.no.dup[,-1]
corr3 <- cor(faa.no.dup$distance,faa.temp2[,-7],use = "pairwise.complete.obs")
corr3 

#modeling
fit <- lm(distance ~speed_ground, data = faa.no.dup)
summary(fit)

fit2 <- lm(distance ~speed_ground + pitch, data = faa.no.dup)
summary(fit2)

fit3 <- lm(distance ~speed_ground + pitch +height , data = faa.no.dup)
summary(fit3)

fit4 <- lm(distance ~speed_ground + pitch +height , data = faa.no.dup)
summary(fit4)

#histogram for airbus and boeing planes 
hist(faa.no.dup$aircraft)


