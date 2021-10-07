#read dataset
data<-read.csv("AB_NYC_2019.csv",header=T,stringsAsFactors = T)
data = data[, !(colnames(data) %in% c("id", "name", "host_id", "host_name", "last_review"))]
data[is.na(data)] <- 0

#fill missing value & drop outliers
data[is.na(data)] <- 0
data<-data[-which(data$price%in%boxplot.stats(data$price)$out),]

#plot correlation
library(GGally)
ggcorr(data, method = c("everything", "pearson")) 

#split data into Training-Testing set
#random sampling
samplesize = 0.90*nrow(data)
set.seed(55)
index=sample(seq_len(nrow(data)), size = samplesize)

#create training and test set
datatrain = data[index,]
datatest = data[-index,]

#Linear Regression Model
model=lm(price~.,data = datatrain)
summary(model)
pred=predict(model,datatest)
testRMSE <- sqrt(mean((pred - datatest$price)^2))
testRMSE
