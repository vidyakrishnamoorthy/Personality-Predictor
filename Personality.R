personality <- read.csv("/Users/littledevil/vk/Fall 2021/BUS 235C/Project/Dataset/archive personality/train.csv",
                        stringsAsFactors = TRUE)

personality <- data.frame(personality)

nrow(personality)
class(personality$Age)
summary(personality)

library(dplyr)
personality<- rename(personality, Personality.Class=Personality..Class.label.)

names(personality)
unique(personality$Personality.Class) 
table(personality$Gender)
table(personality$Personality.Class)

head(personality$Personality.Class)

'''
personality$openness <- scale(personality$openness)
personality$neuroticism <- scale(personality$neuroticism)
personality$conscientiousness <- scale(personality$conscientiousness)
personality$agreeableness <- scale(personality$agreeableness)
personality$extraversion <- scale(personality$extraversion)

personality.glm <- glm(Personality.Class~., family=binomial, data=personality)
step(personality.glm)

summary(personality.glm)
pred_personality <- predict(personality.glm,type = "response")
hist(pred_personality)
plot(x = pred_personality, y = personality$Personality.Class)
abline(0,1)

'''
plot(openness ~ Age, data = personality)
boxplot(personality[,-c(1,2,8)])

cor(personality[,-c(1,2,8)]) * 50

plot(personality[,-c(1,2,8)])

personality.boxplot <- boxplot(personality[,-c(1,8)])
boxplot.stats(personality$Age)$out

personality_updated <- personality[which(personality$Age < 24 & 
                                           personality$Age > 5), ]
personality_updated.boxplot <- boxplot(personality_updated[,-c(1,8)])
boxplot.stats(personality_updated$Age)$out

boxplot(personality)
boxplot.stats(personality$Age)$out

par(mfrow = c(2,3))
hist(personality$openness, labels = T, ylim = c(0, 200))
hist(personality$neuroticism, labels = T, ylim = c(0, 200))
hist(personality$conscientiousness, labels = T, ylim = c(0, 200))
hist(personality$agreeableness, labels = T, ylim = c(0, 200))
hist(personality$extraversion, labels = T, ylim = c(0, 200))

par(mfrow = c(2,3))
hist(personality$openness, freq = TRUE)
hist(personality$neuroticism)
hist(personality$conscientiousness, labels = T, ylim = c(0, 200))
hist(personality$agreeableness, labels = T, ylim = c(0, 200))
hist(personality$extraversion, labels = T, ylim = c(0, 200))

index <- sample(nrow(personality),nrow(personality) * 0.90) 
personality_train = personality[index,] 
personality_test = personality[-index,]

personality_train = read.csv("/Users/littledevil/vk/Fall 2021/BUS 235C/Project/Dataset/archive personality/train.csv",
         stringsAsFactors = TRUE)
personality_test = read.csv("/Users/littledevil/vk/Fall 2021/BUS 235C/Project/Dataset/archive personality/test.csv",
                             stringsAsFactors = TRUE)

personality_train<- rename(personality_train, Personality.Class=Personality..Class.label.)
personality_test<- rename(personality_test, Personality.Class=Personality..class.label.)

personality_train$Personality.Class
personality_test$Personality.Class


# CART
library(rpart)
library(rpart.plot)

loss_matrix = matrix(c(0,5,5,5,5, 5,0,5,5,5, 5,5,0,5,5, 5,5,5,0,5, 5,5,5,5,0), nrow = 5)

personality_rpart0 <- rpart(formula = Personality.Class ~ ., data = personality_train, method = "class") 
personality_rpart <- rpart(formula = Personality.Class ~ . , data = personality_train, method = "class", 
                           parms = list(loss=loss_matrix))


par(mfrow = c(1,1))
rpart.plot(personality_rpart, extra = 1)
prp(personality_rpart, extra = 1)

## prediction
personality_pred <- predict(personality_rpart, type = "class") 
table(personality_train$Personality.Class, personality_pred, dnn = c("Truth", "Predicted"))

personality_test_prob_rpart <- predict(personality_rpart, personality_test, type="prob")


#################################################
#################################################
#################################################

length(personality.train1$Personality.Class)
nrow(personality.pred1$net.result)
length(personality.test1$Personality.Class)
nrow(personality.pred2$net.result)

names(personality.train1$Personality.Class)
names(personality.pred1$net.result)
names(personality.test1$Personality.Class)
names(personality.pred2$net.result)

head(personality.pred1$net.result)
head(personality.train1$Personality.Class)
head(personality.pred2$net.result)
head(personality.test1$Personality.Class)

#################################################
#################################################
#################################################


cost <- function(r, phat)
{ 
  weight0 <- 1
  weight1 <- 10
  pcut <- 0.25 #weight0/((weight1*4)+weight0) 
  mean_values <- array()
  if (r == "dependable")
  {
    cd <-(r=="dependable")&(phat<pcut)
    ce <- (r=="extraverted")&(phat>pcut)
    cl <-(r=="lively")&(phat>pcut)
    cr <- (r=="responsible")&(phat>pcut)
    cs <- (r=="serious")&(phat>pcut)
    append(mean_values, mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)) 
  } else if (r == "extraverted")
  {
    cd <-(r=="dependable")&(phat>pcut)
    ce <- (r=="extraverted")&(phat<pcut)
    cl <-(r=="lively")&(phat>pcut)
    cr <- (r=="responsible")&(phat>pcut)
    cs <- (r=="serious")&(phat>pcut)
    append(mean_values, mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)) 
  } else if (r == "lively")
  {
    cd <-(r=="dependable")&(phat>pcut)
    ce <- (r=="extraverted")&(phat>pcut)
    cl <-(r=="lively")&(phat<pcut)
    cr <- (r=="responsible")&(phat>pcut)
    cs <- (r=="serious")&(phat>pcut)
    append(mean_values, mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)) 
  } else if (r == "responsible")
  {
    cd <-(r=="dependable")&(phat>pcut)
    ce <- (r=="extraverted")&(phat>pcut)
    cl <-(r=="lively")&(phat>pcut)
    cr <- (r=="responsible")&(phat<pcut)
    cs <- (r=="serious")&(phat>pcut)
    append(mean_values, mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)) 
  } else if (r == "serious")
  {
    cd <-(r=="dependable")&(phat>pcut)
    ce <- (r=="extraverted")&(phat>pcut)
    cl <-(r=="lively")&(phat>pcut)
    cr <- (r=="responsible")&(phat>pcut)
    cs <- (r=="serious")&(phat<pcut)
    append(mean_values, mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)) 
  }
}

cost(personality_train$Personality.Class, predict(personality_rpart, personality_train, type="prob"))

personality_class <- personality_train$Personality.Class
predx1 <- predict(personality_rpart, personality_train, type="prob")

personality_class[1]
predx1[1,2]

personality_cost(personality_class, predx1)

a1 = 5
a2 = 0
sum_value = 0

for (ind in length(personality_train$Personality.Class))
{
  if(personality_class[ind] == "dependable")
  {
    sv = a2 * predx1[1,1] + a1 * predx1[1,2] + a1 * predx1[1,3] + a1 * predx1[1,4] + a1 * predx1[1,5]
    sum_value = sum_value + sv
  }
  if(personality_class[ind] == "extraverted")
  {
    sv = a1 * predx1[1,1] + a2 * predx1[1,2] + a1 * predx1[1,3] + a1 * predx1[1,4] + a1 * predx1[1,5]
    sum_value = sum_value + sv
  }
  if(personality_class[ind] == "lively")
  {
    sv = a1 * predx1[1,1] + a1 * predx1[1,2] + a2 * predx1[1,3] + a1 * predx1[1,4] + a1 * predx1[1,5]
    sum_value = sum_value + sv
  }
  if(personality_class[ind] == "responsible")
  {
    sv = a1 * predx1[1,1] + a1 * predx1[1,2] + a1 * predx1[1,3] + a2 * predx1[1,4] + a1 * predx1[1,5]
    sum_value = sum_value + sv
  }
  if(personality_class[ind] == "serious")
  {
    sv = a1 * predx1[1,1] + a1 * predx1[1,2] + a1 * predx1[1,3] + a1 * predx1[1,4] + a2 * predx1[1,5]
    sum_value = sum_value + sv
  }
}

sum_value/length(personality_train$Personality.Class)


cost(personality_train$Personality.Class[c(1,2)], predict(personality_rpart, personality_train[c(2,2),], type="prob"))
print(personality_train$Personality.Class[1])
print(predict(personality_rpart, personality_train, type="prob"))

colnames(predict(personality_rpart, personality_train, type="prob"))
predict(personality_rpart, personality_train, type="prob")[1,]
personality_train$Personality.Class[c(1,2)]

##############
##############
##############
##############

r <- personality_train$Personality.Class
phat <- predict(personality_rpart, personality_train, type="prob")
weight0 <- 1
weight1 <- 5
pcut <- .25 
#print(r)
cd = ce = cl = cr = cs = 0
if (r == "extraverted")
{
  cd <-(r=="dependable")&(phat<pcut)
  ce <- (r=="extraverted")&(phat>pcut)
  cl <-(r=="lively")&(phat>pcut)
  cr <- (r=="responsible")&(phat>pcut)
  cs <- (r=="serious")&(phat>pcut)
#  print(phat, pcut)
#  print(r=="extraverted")&(phat>pcut)
  mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)
}
#print((r=="dependable")&(phat<pcut))
mean(weight0*cd+weight1*ce+weight1*cl+weight1*cr+weight1*cs)

##############
##############
##############
##############

cost(personality_test$Personality.Class, predict(personality_rpart, personality_test, type="prob"))









##################################

personality.train.X<- as.data.frame(scale(model.matrix(~., data = personality_train[,-ncol(personality_train)])[,-1]))
personality.train.Y<- personality_train[,ncol(personality_train)]
personality.train1<- data.frame(Personality.Class = personality.train.Y, personality.train.X)

personality.test.X<- as.data.frame(scale(model.matrix(~., data = personality_test[,-ncol(personality_test)])[,-1]))
personality.test.Y<- personality_test[,ncol(personality_test)]
personality.test1<- data.frame(Personality.Class = personality.test.Y, personality.test.X)

library(neuralnet)
personality.ann <- neuralnet(Personality.Class ~., data = personality.train1, hidden = c(3,2), linear.output = FALSE)
plot(personality.ann)

nrow(personality.pred1$net.result)
nrow(personality_test)

library(neuralnet)
personality.pred2 <- neuralnet::compute(personality.ann, personality.test1)
nna_cost <- personality_cost(personality.test1$Personality.Class, personality.pred2$net.result)
nna_cost
detach(package:neuralnet)
library(ROCR)

personality.pred2.ann<- max(personality.pred2$net.result)
table(personality_train$Personality.Class, personality.pred2.ann, dnn=c("Truth","Predicted"))

head(sort(rownames(personality.pred1$net.result)))
head(sort(rownames(personality.train1)))

head(personality.pred1$net.result)
head(colnames(personality.pred1$net.result)[apply(personality.pred1$net.result,1,which.max)])

nrow(personality.pred1$net.result)

colnames(personality.pred1$net.result) <- c("dependable", "extraverted", "lively", "responsible", "serious")

colnames(personality.pred2$net.result)

head(cbind(personality_train$Personality.Class, personality.pred1$net.result), 10)
detach(package:neuralnet)
####### ROC curve and AUC
library(ROCR)
pred <- prediction(personality.pred2$net.result, personality_test$Personality.Class)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

head(personality_train$Personality.Class)
head(colnames(personality.pred1$net.result)[apply(personality.pred1$net.result,1,which.max)])

table(personality_train$Personality.Class,
      colnames(personality.pred1$net.result)[apply(personality.pred1$net.result,1,which.max)], 
      dnn=c("Truth","Predicted"))

mean(personality_train$Personality.Class)
mean(personality.pred2$net.result)

personality.pred1.ann<- (personality.pred1$net.result>mean(personality.pred1$net.result))*1

nrow(personality.pred1.ann)

table(personality_train$Personality.Class, personality.pred1.ann, dnn=c("Truth","Predicted"))
