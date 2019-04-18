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