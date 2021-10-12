personality <- read.csv("/Users/littledevil/vk/Fall 2021/BUS 235C/Project/Dataset/personality/personality dataset.csv",
                        stringsAsFactors = TRUE)
personality <- data.frame(personality)
library(dplyr)
library(rpart)
library(rpart.plot)

personality<- rename(personality, Personality.Class=Personality..Class.label.)

table(is.na(personality))

personality_index <- sample(nrow(personality),nrow(personality) * 0.80) 
personality_train = personality[personality_index, ] 
personality_test = personality[-personality_index,]
table(personality_train$Personality.Class)


table(personality_train$Personality.Class)

loss_matrix = matrix(c(0,5,5,5,5, 5,0,5,5,5, 5,5,0,5,5, 5,5,5,0,5, 5,5,5,5,0), nrow = 5)
personality_full_rpart <- rpart(formula = Personality.Class ~ . , data = personality, method = "class", 
                           parms = list(loss=loss_matrix))

par(mfrow = c(1,1))
rpart.plot(personality_full_rpart, extra = 1)
prp(personality_full_rpart, extra = 1)

#############
#############
#### EDA ####
#############
#############
nrow(personality)
summary(personality)
table(personality$Gender)
table(personality$Personality.Class)
boxplot(personality[,-c(1,2,8)])

sapply(personality, class)

nrow(personality_train)
nrow(personality_test)

par(mfrow = c(2,3))
hist(personality$openness, labels = T, ylim = c(0, 200))
hist(personality$neuroticism, labels = T, ylim = c(0, 200))
hist(personality$conscientiousness, labels = T, ylim = c(0, 200))
hist(personality$agreeableness, labels = T, ylim = c(0, 200))
hist(personality$extraversion, labels = T, ylim = c(0, 200))

par(mfrow = c(1,1))

#########################################
#########################################
#### multinomial logistic regression ####
#########################################
#########################################

cor(personality[-c(1,2,8)])

colnames(personality[-c(1,2,8)])

table(personality_train$Personality.Class)
require(nnet)

personality_multinom <- multinom(Personality.Class ~ ., data = personality_train)

colnames(personality_multinom$fitted.values)[apply(personality_multinom$fitted.values,1,which.max)]

summary(personality_multinom)
plot(summary(personality_multinom)$coefficients[4,c(5:9)], type = "b", col = "red")
lines(summary(personality_multinom)$coefficients[4,c(5:9)], type = "b", col = "blue")
lines(summary(personality_multinom)$coefficients[4,c(5:9)], type = "b", col = "green")
lines(summary(personality_multinom)$coefficients[4,c(5:9)], type = "b", col = "yellow")

## prediction
personality_multinom_train_pred <- predict(personality_multinom, type = "class") 
table(personality_train$Personality.Class, personality_multinom_train_pred, dnn = c("Truth", "Predicted"))
personality_multinom_train_pred <- predict(personality_multinom, type = "prob") 
head(personality_multinom_train_pred)
multinom_cost <- personality_cost(personality_train$Personality.Class, personality_multinom_train_pred)
multinom_cost

personality_multinom_test_pred <- predict(personality_multinom, newdata = personality_test, type = "class") 
table(personality_test$Personality.Class, personality_multinom_test_pred, dnn = c("Truth", "Predicted"))
personality_multinom_test_pred <- predict(personality_multinom, newdata = personality_test, type = "prob") 
multinom_cost <- personality_cost(personality_test$Personality.Class, personality_multinom_test_pred)
multinom_cost

#############################
#############################
#### Classification tree ####
#############################
#############################

library(rpart)
library(rpart.plot)
loss_matrix = matrix(c(0,5,5,5,5, 5,0,5,5,5, 5,5,0,5,5, 5,5,5,0,5, 5,5,5,5,0), nrow = 5)
personality_rpart <- rpart(formula = Personality.Class ~ . , data = personality_train, method = "class", 
                           parms = list(loss=loss_matrix))

par(mfrow = c(1,1))
rpart.plot(personality_rpart, extra = 1)
prp(personality_rpart, extra = 1)

## prediction
personality_cart_train_pred <- predict(personality_rpart, type = "class") 
table(personality_train$Personality.Class, personality_cart_train_pred, dnn = c("Truth", "Predicted"))
personality_cart_train_pred <- predict(personality_rpart, type = "prob") 
head(personality_cart_train_pred)
cart_cost <- personality_cost(personality_train$Personality.Class, personality_cart_train_pred)
cart_cost

personality_cart_test_pred <- predict(personality_rpart, newdata = personality_test, type = "class") 
table(personality_test$Personality.Class, personality_cart_test_pred, dnn = c("Truth", "Predicted"))
personality_cart_test_pred <- predict(personality_rpart, newdata = personality_test, type = "prob") 
cart_cost <- personality_cost(personality_test$Personality.Class, personality_cart_test_pred)
cart_cost

head(personality_cart_test_pred)
head(personality_train)

#########################
#########################
#### neural networks ####
#########################
#########################

personality.train.X<- as.data.frame(scale(model.matrix(~., data = personality_train[,-ncol(personality_train)])[,-1]))
personality.train.Y<- personality_train[,ncol(personality_train)]
personality.train1<- data.frame(Personality.Class = personality.train.Y, personality.train.X)

personality.test.X<- as.data.frame(scale(model.matrix(~., data = personality_test[,-ncol(personality_test)])[,-1]))
personality.test.Y<- personality_test[,ncol(personality_test)]
personality.test1<- data.frame(Personality.Class = personality.test.Y, personality.test.X)

library(neuralnet)
personality.ann <- neuralnet(Personality.Class ~., data = personality.train1, hidden = c(5,4), linear.output = FALSE)
plot(personality.ann, cex = 0.5)

personality.pred1 <- neuralnet::compute(personality.ann, personality.train1)
nna_cost <- personality_cost(personality.train1$Personality.Class, personality.pred1$net.result)
nna_cost

colnames(personality.pred1$net.result) <- c("dependable", "extraverted", "lively", "responsible", "serious")

table(personality_train$Personality.Class,
      colnames(personality.pred1$net.result)[apply(personality.pred1$net.result,1,which.max)], 
      dnn=c("Truth","Predicted"))

library(neuralnet)
personality.pred2 <- neuralnet::compute(personality.ann, personality.test1)
nna_cost <- personality_cost(personality.test1$Personality.Class, personality.pred2$net.result)
nna_cost

colnames(personality.pred2$net.result) <- c("dependable", "extraverted", "lively", "responsible", "serious")

table(personality_test$Personality.Class,
      colnames(personality.pred2$net.result)[apply(personality.pred2$net.result,1,which.max)], 
      dnn=c("Truth","Predicted"))



#######################
#######################
#### Cost function ####
#######################
#######################

#   d e l r s
# d 0 3 3 3 5
# e 3 0 3 5 5 
# l 3 3 0 5 5
# r 3 5 5 0 3
# s 5 5 5 3 0

personality_cost <- function(personality_class, predx1)
{
  a5 = 5
  a3 = 3
  a0 = 0
  sum_value = 0
  for (ind in length(personality_class))
  {
    if(personality_class[ind] == "dependable")
    {
      sv = a0 * predx1[ind,1] + a3 * predx1[ind,2] + a3 * predx1[ind,3] + a3 * predx1[ind,4] + a5 * predx1[ind,5]
      sum_value = sum_value + sv
    }
    if(personality_class[ind] == "extraverted")
    {
      sv = a3 * predx1[ind,1] + a0 * predx1[ind,2] + a3 * predx1[ind,3] + a5 * predx1[ind,4] + a5 * predx1[ind,5]
      sum_value = sum_value + sv
    }
    if(personality_class[ind] == "lively")
    {
      sv = a3 * predx1[ind,1] + a3 * predx1[ind,2] + a0 * predx1[ind,3] + a5 * predx1[ind,4] + a5 * predx1[ind,5]
      sum_value = sum_value + sv
    }
    if(personality_class[ind] == "responsible")
    {
      sv = a3 * predx1[ind,1] + a5 * predx1[ind,2] + a5 * predx1[ind,3] + a0 * predx1[ind,4] + a3 * predx1[ind,5]
      sum_value = sum_value + sv
    }
    if(personality_class[ind] == "serious")
    {
      sv = a5 * predx1[ind,1] + a5 * predx1[ind,2] + a5 * predx1[ind,3] + a3 * predx1[ind,4] + a0 * predx1[ind,5]
      sum_value = sum_value + sv
    }
  }
  return(sum_value)
}

