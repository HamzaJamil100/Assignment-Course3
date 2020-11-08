library(dplyr)

##Train##

x_train <- read.table(file = "X_train.txt", sep="", dec = ".")
y_train <- read.table(file = "y_train.txt", sep="", dec = ".")

body_acc_x_train <- read.table(file = "body_acc_x_train.txt", sep="", dec = ".")
body_acc_y_train <- read.table(file = "body_acc_y_train.txt", sep="", dec = ".")
body_acc_z_train <- read.table(file = "body_acc_z_train.txt", sep="", dec = ".")

body_gyro_x_train <- read.table(file = "body_gyro_x_train.txt", sep="", dec = ".")
body_gyro_y_train <- read.table(file = "body_gyro_y_train.txt", sep="", dec = ".")
body_gyro_z_train <- read.table(file = "body_gyro_z_train.txt", sep="", dec = ".")

total_acc_y_train <- read.table(file = "total_acc_y_train.txt", sep="", dec = ".")
total_acc_x_train <- read.table(file = "total_acc_x_train.txt", sep="", dec = ".")
total_acc_z_train <- read.table(file = "total_acc_z_train.txt", sep="", dec = ".")

##Test##

x_test <- read.table(file = "X_train.txt", sep="", dec = ".")
y_test <- read.table(file = "y_train.txt", sep="", dec = ".")

body_acc_x_test <- read.table(file = "body_acc_x_test.txt", sep="", dec = ".")
body_acc_y_test <- read.table(file = "body_acc_y_test.txt", sep="", dec = ".")
body_acc_z_test <- read.table(file = "body_acc_z_test.txt", sep="", dec = ".")

body_gyro_x_test <- read.table(file = "body_gyro_x_test.txt", sep="", dec = ".")
body_gyro_y_test <- read.table(file = "body_gyro_y_test.txt", sep="", dec = ".")
body_gyro_z_test <- read.table(file = "body_gyro_z_test.txt", sep="", dec = ".")

total_acc_y_test <- read.table(file = "total_acc_y_test.txt", sep="", dec = ".")
total_acc_x_test <- read.table(file = "total_acc_x_test.txt", sep="", dec = ".")
total_acc_z_test <- read.table(file = "total_acc_z_test.txt", sep="", dec = ".")


##Mergeddata##

x_train_test <- merge(x_train,x_test,all = TRUE)
y_train_test <- merge(y_train,y_test,all = TRUE)

body_acc_x_train_test <- merge(body_acc_x_train,body_acc_x_test,all = TRUE)
body_acc_y_train_test <- merge(body_acc_y_train,body_acc_y_test,all = TRUE)
body_acc_z_train_test <- merge(body_acc_z_train,body_acc_z_test,all = TRUE)

body_gyro_x_train_test <- merge(body_gyro_x_train,body_gyro_x_test,all = TRUE)
body_gyro_y_train_test <- merge(body_gyro_y_train,body_gyro_y_test,all = TRUE)
body_gyro_z_train_test <- merge(body_gyro_z_train,body_gyro_z_test,all = TRUE)

total_acc_x_train_test <- merge(total_acc_x_train,total_acc_x_test,all = TRUE)
total_acc_y_train_test <- merge(total_acc_y_train,total_acc_y_test,all = TRUE)
total_acc_z_train_test <- merge(total_acc_z_train,total_acc_z_test,all = TRUE)


##ColMeans##

ColMean_x_train_test<- colMeans(y_train_test[sapply(y_train_test, is.numeric)])
ColMean_y_train_test<- colMeans(x_train_test[sapply(x_train_test, is.numeric)])

ColMean_body_acc_x_train_test<- colMeans(body_acc_x_train_test[sapply(body_acc_x_train_test, is.numeric)])
ColMean_body_acc_y_train_test<- colMeans(body_acc_y_train_test[sapply(body_acc_y_train_test, is.numeric)])
ColMean_body_acc_z_train_test<- colMeans(body_acc_z_train_test[sapply(body_acc_z_train_test, is.numeric)])

ColMean_body_gyro_x_train_test<- colMeans(body_gyro_x_train_test[sapply(body_gyro_x_train_test, is.numeric)])
ColMean_body_gyro_y_train_test<- colMeans(body_gyro_y_train_test[sapply(body_gyro_y_train_test, is.numeric)])
ColMean_body_gyro_z_train_test<- colMeans(body_gyro_z_train_test[sapply(body_gyro_z_train_test, is.numeric)])

ColMean_total_acc_x_train_test<- colMeans(total_acc_x_train_test[sapply(total_acc_x_train_test, is.numeric)])
ColMean_total_acc_y_train_test<- colMeans(total_acc_y_train_test[sapply(total_acc_y_train_test, is.numeric)])
ColMean_total_acc_z_train_test<- colMeans(total_acc_z_train_test[sapply(total_acc_z_train_test, is.numeric)])

##ColSD##
ColSD_x_train_test<- x_train_test %>% summarise_if(is.numeric, sd)
ColSD_y_train_test<- y_train_test %>% summarise_if(is.numeric, sd)

ColSD_body_acc_x_train_test<- body_acc_x_train_test %>% summarise_if(is.numeric, sd)
ColSD_body_acc_y_train_test<- body_acc_y_train_test %>% summarise_if(is.numeric, sd)
ColSD_body_acc_z_train_test<- body_acc_z_train_test %>% summarise_if(is.numeric, sd)

ColSD_body_gyro_x_train_test<- body_gyro_x_train_test %>% summarise_if(is.numeric, sd)
ColSD_body_gyro_y_train_test<- body_gyro_y_train_test %>% summarise_if(is.numeric, sd)
ColSD_body_gyro_z_train_test<- body_gyro_z_train_test %>% summarise_if(is.numeric, sd)

ColSD_total_acc_x_train_test<- total_acc_x_train_test %>% summarise_if(is.numeric, sd)
ColSD_total_acc_y_train_test<- total_acc_y_train_test %>% summarise_if(is.numeric, sd)
ColSD_total_acc_z_train_test<- total_acc_z_train_test %>% summarise_if(is.numeric, sd)

##Average of ColMean##

ColMean_Average_x_train_test<- colMeans(ColMean_y_train_test[sapply(y_train_test, is.numeric)])
ColMean_Average_y_train_test<- colMeans(ColMean_x_train_test[sapply(x_train_test, is.numeric)])

ColMean_Average_body_acc_x_train_test<- colMeans(ColMean_body_acc_x_train_test[sapply(body_acc_x_train_test, is.numeric)])
ColMean_Average_body_acc_y_train_test<- colMeans(ColMean_body_acc_y_train_test[sapply(body_acc_y_train_test, is.numeric)])
ColMean_Average_body_acc_z_train_test<- colMeans(ColMean_body_acc_z_train_test[sapply(body_acc_z_train_test, is.numeric)])

ColMean_Average_body_gyro_x_train_test<- colMeans(ColMean_body_gyro_x_train_test[sapply(body_gyro_x_train_test, is.numeric)])
ColMean_Average_body_gyro_y_train_test<- colMeans(ColMean_body_gyro_y_train_test[sapply(body_gyro_y_train_test, is.numeric)])
ColMean_Average_body_gyro_z_train_test<- colMeans(ColMean_body_gyro_z_train_test[sapply(body_gyro_z_train_test, is.numeric)])

ColMean_Average_total_acc_x_train_test<- colMeans(ColMean_total_acc_x_train_test[sapply(total_acc_x_train_test, is.numeric)])
ColMean_Average_total_acc_y_train_test<- colMeans(ColMean_total_acc_y_train_test[sapply(total_acc_y_train_test, is.numeric)])
ColMean_Average_total_acc_z_train_test<- colMeans(ColMean_total_acc_z_train_test[sapply(total_acc_z_train_test, is.numeric)])

##Average of ColSD##

ColSD_Average_x_train_test<- colMeans(ColSD_y_train_test[sapply(y_train_test, is.numeric)])
ColSD_Average_y_train_test<- colMeans(ColSD_x_train_test[sapply(x_train_test, is.numeric)])

ColSD_Average_body_acc_x_train_test<- colMeans(ColSD_body_acc_x_train_test[sapply(body_acc_x_train_test, is.numeric)])
ColSD_Average_body_acc_y_train_test<- colMeans(ColSD_body_acc_y_train_test[sapply(body_acc_y_train_test, is.numeric)])
ColSD_Average_body_acc_z_train_test<- colMeans(ColSD_body_acc_z_train_test[sapply(body_acc_z_train_test, is.numeric)])

ColSD_Average_body_gyro_x_train_test<- colMeans(ColSD_body_gyro_x_train_test[sapply(body_gyro_x_train_test, is.numeric)])
ColSD_Average_body_gyro_y_train_test<- colMeans(ColSD_body_gyro_y_train_test[sapply(body_gyro_y_train_test, is.numeric)])
ColSD_Average_body_gyro_z_train_test<- colMeans(ColSD_body_gyro_z_train_test[sapply(body_gyro_z_train_test, is.numeric)])

ColSD_Average_total_acc_x_train_test<- colMeans(ColSD_total_acc_x_train_test[sapply(total_acc_x_train_test, is.numeric)])
ColSD_Average_total_acc_y_train_test<- colMeans(ColSD_total_acc_y_train_test[sapply(total_acc_y_train_test, is.numeric)])
ColSD_Average_total_acc_z_train_test<- colMeans(ColSD_total_acc_z_train_test[sapply(total_acc_z_train_test, is.numeric)])



# 
# head(d, 2)
# x_test[1:10,1:5]
# x_train[1:10,1:5]
# 
# data1 <- data.frame(z2=c(1,1,1), z3=c(4,4,4), z1 = c(7,7,7))
# data2 <- data.frame(z2=c(1,1,1), z3=c(4,5,4), z1 = c(7,7,7))
# mergeddata <- merge(data1,data2,all = TRUE)
# mergeddata */