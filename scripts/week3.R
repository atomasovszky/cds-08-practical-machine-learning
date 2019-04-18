source("global.R")


# classification trees —————————————————————————————————————————————————————————
data(iris)
iris <- data.table(iris)

iris[, .N, Species]

ggplot(iris, aes(Petal.Width, Sepal.Width, colour = Species)) +
    geom_point(size = 3, alpha = .55)

train <- createDataPartition(
    y    = iris$Species,
    p    = .75,
    list = FALSE
)

training <- iris[train]
testing <- iris[-train]
modfit <- train(Species ~ ., method = "rpart", data = training)
print(modfit$finalModel)

plot(modfit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modfit$finalModel, use.n = TRUE, all = TRUE, cex = .8)

library(rattle)
fancyRpartPlot(modfit$finalModel)

predict(modfit, data = testing)



# Bagging ——————————————————————————————————————————————————————————————————————

library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- data.table(ozone)[order(ozone)]

ll <- matrix(NA, nrow = 10, ncol = 155)
for (i in 1:10) {
    ss <- sample(1:dim(ozone)[1], replace = TRUE)
    ozone0 <- ozone[ss,]
    ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
    ll[i,] <- predict(loess0, newdata = data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)


# random forest ————————————————————————————————————————————————————————————————

data(iris)
library(randomForest)
iris <- data.table(iris)

train <- createDataPartition(
    y    = iris$Species,
    p    = .7,
    list = FALSE
)

training <- iris[train]
testing <- iris[-train]

model <- train(Species ~ ., method = "rf", prox = TRUE, data = training)
model

getTree(model$finalModel, k = 2)

irisP <- classCenter(training[,c(3,4)], training$Species, model$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)


pred <- predict(model, testing)
table(pred, testing$Species)
testing$predRight <- pred==testing$Species
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")



# Boosting —————————————————————————————————————————————————————————————————————

library(ISLR)
data(Wage)
Wage <- data.table(Wage)
Wage[, logwage := NULL]

train <- createDataPartition(
    y    = Wage$wage,
    p    = .7,
    list = FALSE
)

training <- Wage[train]
testing <- Wage[-train]

modFit <- train(wage ~ ., method = "gbm", data = training, verbose = FALSE)
modFit

qplot(predict(modFit, testing), wage, data = testing)


# Quiz —————————————————————————————————————————————————————————————————————————

# problem 1
library(AppliedPredictiveModeling)
library(rattle)
data(segmentationOriginal)

segmentationOriginal <- data.table(segmentationOriginal)

training <- segmentationOriginal[Case == "Train"][, Case := NULL][]
testing  <- segmentationOriginal[Case == "Test"][, Case := NULL][]

set.seed(125)

modelFit <- train(Class ~ ., data = training, method = "rpart")
fancyRpartPlot(modelFit$finalModel)


# problem 3
library(pgmm)
data(olive)
olive <- olive[,-1]
olive <- data.table(olive)
newdata <- as.data.frame(t(colMeans(olive)))

rpart <- train(Area ~ ., data = olive, method = "rpart")
predict(rpart, newdata = newdata)
fancyRpartPlot(rpart$finalModel)


# problem 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
glm <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family = "binomial")
summary(glm$finalModel)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, predict(glm, testSA))
missClass(trainSA$chd, predict(glm, trainSA))


# problem 5
library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test)

vowel.test <- data.table(vowel.test)
vowel.train <- data.table(vowel.train)
vowel.train[, y := as.factor(y)]
vowel.test[, y := as.factor(y)]

set.seed(33833)
rf <- randomForest(y ~ ., data = vowel.train)
caret::varImp(rf)
