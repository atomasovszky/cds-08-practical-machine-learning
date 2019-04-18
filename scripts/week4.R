source("global.R")

# QUIZ —————————————————————————————————————————————————————————————————————————


# problem 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test <- data.table(vowel.test)
vowel.train <- data.table(vowel.train)

vowel.test[, y := as.factor(y)]
vowel.train[, y := as.factor(y)]

set.seed(33833)

model1 <- train(y ~ ., data = vowel.train, method = "rf")
model2 <- train(y ~ ., data = vowel.train, method = "gbm")

cm1 <- confusionMatrix(predict(model1, vowel.test), vowel.test$y)
cm2 <- confusionMatrix(predict(model2, vowel.test), vowel.test$y)
cm1$overall[1]
cm2$overall[1]

data.table(
    pred_model_1 = predict(model1, vowel.test),
    pred_model_2 = predict(model2, vowel.test),
    test_value   = vowel.test$y
) %>%
    .[pred_model_1 == pred_model_2, confusionMatrix(pred_model_1, test_value)]


# problem 2
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

model_rf <- train(diagnosis ~ ., data  = training, method = "rf")
model_gbm <- train(diagnosis ~ ., data  = training, method = "gbm")
model_lda <- train(diagnosis ~ ., data  = training, method = "lda")

confusionMatrix(predict(model_rf, testing), testing$diagnosis)$overall[1]
confusionMatrix(predict(model_gbm, testing), testing$diagnosis)$overall[1]
confusionMatrix(predict(model_lda, testing), testing$diagnosis)$overall[1]

predicts <- data.table(
    id  = 1:nrow(testing),
    rf  = predict(model_rf, testing),
    gbm = predict(model_gbm, testing),
    lda = predict(model_lda, testing),
    truth = testing$diagnosis
) %>% melt(id.vars = c("id", "truth"))

predicts %>%
    .[, .N, .(id, truth, value)] %>%
    .[, value := as.factor(value)] %>%
    .[, voted := N == max(N), .(id, truth)] %>%
    .[voted == TRUE] %>%
    .[, confusionMatrix(value, truth)]


# problem 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

library(elasticnet)
lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
plot(lasso$finalModel)
?plot.enet


# problem 4