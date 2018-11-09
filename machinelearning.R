# This library contains the logistic regression clasifier
library(class)
## preparing the data
library(devtools)
library(datamicroarray)
library(MASS)
data("alon", package = "datamicroarray")
status = as.character(alon$y)
write.csv(alon$x,"alondata1.csv")
alondata1 = read.csv("alondata1.csv",header = TRUE, sep = ",")
alondata1 = alondata1[,-1]
alondata = data.frame(alondata1, status)
write.csv(alondata,"alondata.csv" )
alondata = read.csv("alondata.csv", header = TRUE, sep = ",")
alondata = alondata[,-1]

## PCA

alon.pca = prcomp(alondata[,1:2000], scale = TRUE)
plot(alon.pca,type = "l")
summary(alon.pca)
scores = alon.pca$rotation[,1]
genescores = abs(scores)
genescorerank = sort(genescores,decreasing = TRUE)
top61 = as.character(names(genescorerank[1:61]))
write.csv(top61,"top61.csv")

## preparing data
top61[62] = "status"
data100 = alondata[,top61]
write.csv(data100, "data100.csv")
data100 = read.csv("data100.csv", header = TRUE, sep = ",")
data100 = data100[,-1]
View(data100)
## bootstrap funciton

bt = function(data, number) {
  data = as.data.frame(data)
 index = sample(c(1:nrow(data)), number, replace = TRUE)
 newdata = data[index,]
 return(newdata)
}

## bootstrap and divide data
set.seed(1)
testindex = sample(c(1:nrow(data100)),22,replace = FALSE )
testdata = subset(data100,rownames(data100) %in% testindex)
traindata =subset(data100,!(rownames(data100) %in% testindex))

bttrain = bt(traindata, 1000)
bttest = bt(testdata,200)

## knn after bootstrap
library("DMwR")
nn3 <- kNN(status ~ .,bttrain,bttest, norm=TRUE,k=3)
TP = sum(nn3 == "t" & bttest$status == 't')
TP
# Type 1 errors, or false positives
Type1 = sum(nn3 == "t" & bttest$status == 'n')
Type1
# Type 2 errors, or false negatives
Type2 = sum(nn3 == "n" & bttest$status == 't')
Type2
# True negatives
TN = sum(nn3 == "n" & bttest$status == 'n')
TN 
mean(nn3 == bttest$status)
nn5 <- kNN(status ~ .,bttrain,bttest,norm=TRUE,k=5)

TP = sum(nn5 == "t" & bttest$status == 't')
TP
# Type 1 errors, or false positives
Type1 = sum(nn5 == "t" & bttest$status == 'n')
Type1
# Type 2 errors, or false negatives
Type2 = sum(nn5 == "n" & bttest$status == 't')
Type2
# True negatives
TN = sum(nn5 == "n" & bttest$status == 'n')
TN 
mean(nn5 == bttest$status)
## logistic regression
log.fit = glm(status ~ .,
              data=bttrain, family = binomial)
log.fit2 = lda(factor(status)~X1264 +X603 + X671 + X375+
                X1753 + X647 + X761 + X1192 + X637 + X1205
              , data = bttrain)
summary(log.fit)
glm.probs <- predict(log.fit, 
                     newdata = bttest, 
                     type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "t", "n")
table(glm.pred,bttest$status)
mean(glm.pred == bttest$status)


## lda
set.seed(1)
attach(bttrain)
lda.fit = lda(factor(status)~X1264 +X603 + X671 + X375+
                X1753 + X647 + X761 + X1192 + X637 + X1205
              , data = bttrain)
lda.fit2 = lda(status~., data = bttrain)
lda.pred = predict(lda.fit,newdata = bttest)
table(lda.pred$class == bttest$status)
mean(lda.pred$class==bttest$status)
