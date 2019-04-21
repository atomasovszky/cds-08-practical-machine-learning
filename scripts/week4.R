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
)

model_comb <- train(truth ~ rf + gbm + lda, data = predicts, method = "rf")
predicts[, combined := predict(model_comb, predicts)]

predicts[, confusionMatrix(rf, truth)]$overall[1]
predicts[, confusionMatrix(gbm, truth)]$overall[1]
predicts[, confusionMatrix(lda, truth)]$overall[1]
predicts[, confusionMatrix(combined, truth)]$overall[1]

predicts %>%
    melt(id.vars = c("id", "truth")) %>%
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
dat <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
library(lubridate) # For year() function below
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)

model <- bats(training$visitsTumblr)
plot(model)

model
predict <- forecast(model, nrow(testing)) %>% as.data.table

data.table(
    predict,
    testing
) %>%
    .[, .N / nrow(testing), .(visitsTumblr >= `Lo 95` & visitsTumblr <= `Hi 95`)]


#problem 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
concrete <- data.table(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain]

set.seed(325)

svm <- e1071::svm(CompressiveStrength ~ ., data = training)
data.table(
    prediction = predict(svm, testing),
    truth      = testing$CompressiveStrength
) %>%
    .[, (prediction - truth)] %>%
    .^2 %>%
    sum %>%
    sqrt %>%
    sqrt