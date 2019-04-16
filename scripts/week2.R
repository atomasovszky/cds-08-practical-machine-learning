source("global.R")


# Example 1 ————————————————————————————————————————————————————————————————————
library(kernlab)
data(spam)
inTrain <- createDataPartition(
    y    = spam$type,
    p    = .75,
    list = FALSE
)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(32343)
modelFit <- train(type ~ ., data = training, method = "glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit, testing)

confusionMatrix(predictions, testing$type)


# Example - K-fold —————————————————————————————————————————————————————————————
set.seed(32323)
folds <- createFolds(
    y           = spam$type,
    k           = 10,
    list        = TRUE,
    returnTrain = TRUE
)
sapply(folds, length)
folds[[1]][1:10]

folds <- createFolds(
    y           = spam$type,
    k           = 10,
    list        = TRUE,
    returnTrain = FALSE
)
sapply(folds, length)
folds[[1]][1:10]



# Example - Resampling —————————————————————————————————————————————————————————
folds <- createResample(
    y           = spam$type,
    times       = 10,
    list        = TRUE
)
sapply(folds, length)
folds[[1]][1:10]



# Example - Time slices ————————————————————————————————————————————————————————
tme <- 1:1000
folds <- createTimeSlices(
    y             = tme,
    initialWindow = 20,
    horizon       = 10
)
names(folds)
folds$train[1:10]



# Training options —————————————————————————————————————————————————————————————
data(spam)
inTrain <- createDataPartition(
    y    = spam$type,
    p    = .75,
    list = FALSE
)

training <- spam[inTrain,]
testing <- spam[-inTrain,]


args(train)
args(trainControl)



# Plotting predictors ——————————————————————————————————————————————————————————
library(ISLR)
data(Wage)
# summary(Wage)

inTrain <- createDataPartition(
    y    = Wage$wage,
    p    = .7,
    list = FALSE
)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# not really useful
featurePlot(x=training[,c("age","education","jobclass")], y = training$wage,
plot="pairs")

qplot(age,wage,data=training)

qplot(age,wage,colour=jobclass, data=training)

p1 <- qplot(age,wage,colour=education, data=training)
p1 + geom_smooth(method = "lm", se = FALSE)

library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutwage)


p1 <- qplot(cutWage,age, data=training,fill=cutWage, geom=c("boxplot"))
p1

p2 <- qplot(cutWage,age, data=training,fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)



t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)


qplot(wage,colour=education,data=training,geom="density")



# Preprocessing ————————————————————————————————————————————————————————————————
data(spam)
inTrain <- createDataPartition(
    y    = spam$type,
    p    = .75,
    list = FALSE
)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

preObj <- preProcess(training[, -58], method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

# apply preprocessing to testing set
testCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


# transforming predictors in order to be normally distributed
preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)


# imputing using KNN
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth‐mean(capAveTruth))/sd(capAveTruth)



# Covariate creation ———————————————————————————————————————————————————————————
library(kernlab)
data(Wage)
inTrain <- createDataPartition(
    y    = Wage$wage,
    p    = .7,
    list = FALSE
)

training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

# dummying
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# near-zero variables
nzv <- nearZeroVar(training, saveMetrics = TRUE)
nzv

# splines
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis

lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = .5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = .5)

# !!!
predict(bsBasis, newdata = testing$age)



# PCA ——————————————————————————————————————————————————————————————————————————
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(
    y    = spam$type,
    p    = .75,
    list = FALSE
)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

# highly correlated variables
names(spam)[c(32,34)]
plot(spam[, 32], spam[, 34])

# small example
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

prComp$rotation

data(spam)
typeColor <- ((spam$type == "spam") * 1 + 1)
preProc <- preProcess(log10(spam[, -58] + 1), method = "pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[, -58] + 1))
plot(spamPC[, 1], spamPC[, 2], col = typeColor)


# modelling with PCA
preProc <- preProcess(log10(training[, -58] + 1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[, -58] + 1))
modelFit <- train(training$type ~ ., method = "glm", data = trainPC)

testPC <- predict(preProc, log10(testing[, -58] + 1))
confusionMatrix(testing$type, predict(modelFit, testPC))


# Prediction using linear regression models ————————————————————————————————————
data(faithful)
inTrain <- createDataPartition(
    y    = faithful$waiting,
    p    = .5,
    list = FALSE
)

trainFaith <- faithful[inTrain, ]
testFaith <- faithful[-inTrain, ]

head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting, lm1$fitted, lwd = 3)

coef(lm1)[1] + coef(lm1)[2] * 80
predict(lm1, newdata = data.frame(waiting = 80))

par(mfrow = c(1, 2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)
dev.off()

# RMSE
sqrt(sum((lm1$fitted - trainFaith$eruptions) ^ 2))
sqrt(sum((predict(lm1, testFaith) - testFaith$eruptions) ^ 2))


# prediction intervals
pred1 <- predict(lm1, newdata = testFaith, interval = "prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

# using caret
modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit)


# Regression with multiple covariates ——————————————————————————————————————————
library(ISLR)
data(Wage)
Wage <- subset(Wage, select = -c(logwage))
summary(Wage)

data(Wage)
inTrain <- createDataPartition(
    y    = Wage$wage,
    p    = .7,
    list = FALSE
)

training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

qplot(age,wage,data=training)
qplot(age,wage,data=training, color = jobclass)
qplot(age,wage,data=training, color = education)

modFit <- train(
    form   = wage ~ age + jobclass + education,
    method = "lm",
    data   = training
)

finMod <- modFit$finalModel
print(modFit)

plot(finMod, 1, pch = 19, cex = .5, col = "#00000010")

# few outliers. check if they can be explained by variables left out
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

# plot by index
plot(finMod$residuals, pch = 19)

pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

# all covariates
modFitAll <- train(wage ~ ., data = training, method = "lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)
summary(modFitAll)





# Quiz 2 ———————————————————————————————————————————————————————————————————————

## problem 2
library(AppliedPredictiveModeling)
library(Hmisc)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = as.data.table(mixtures[ inTrain,])
testing = mixtures[-inTrain,]

trainMelt <- training %>%
    melt(id.vars = c("index", "CompressiveStrength")) %>%
    .[, value_cut := cut2(value, g = 2), variable]

p <- map(setdiff(names(training), "CompressiveStrength"), ~{
    training %>%
        ggplot(aes_string("index", "CompressiveStrength", colour = .x)) +
            geom_point(alpha = .5, size = 2) +
            labs(colour = .x)
})

p_cut <- map(setdiff(names(training), "CompressiveStrength"), ~{
    cut_var_name <- glue("{.x}_cut")
    p1 <- training %>%
        .[, (cut_var_name) := cut2(get(.x), g = 2)] %>%
        ggplot(aes_string("index", "CompressiveStrength", colour = cut_var_name)) +
            geom_point(alpha = .5, size = 2) +
            labs(colour = .x)
})


do.call("grid.arrange", c(p_cut, ncol=3))



## problem 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
ggplot(training, aes(Superplasticizer)) + geom_histogram(alpha = .5)



# problem 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

pcvars <- grep("^IL", names(training), value = TRUE)
preProcess(training[, pcvars], method = "pca", thresh = .9)



# problem 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

model1 <- train(
    form   = diagnosis ~ .,
    data   = training[, c(pcvars, "diagnosis")],
    method = "glm"
)

trainPCA <- preProcess(training[, pcvars], method = "pca", thresh = .8)

model2 <- train(
    form   = diagnosis ~ .,
    data = training[, c(pcvars, "diagnosis")],
    method = "glm",
    preprocessing = "pca",
    thresh = .8
)

confusionMatrix(model1, newdata = predict(trainPCA, testing[, c(pcvars, "diagnosis")]))