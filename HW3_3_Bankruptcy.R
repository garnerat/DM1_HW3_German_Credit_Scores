# HW3 Problem 3
# Bankruptcy Data - use cutoff probability 1/16

# Load Data

data.bankruptcy <- read.csv("C:/Users/atg530/Desktop/Grad School/Spring 2017/Data Mining/Data Mining I/Course Documents/Data/bankruptcy.csv")

head(data.bankruptcy)

##### Partition Train/Test #####


index <- sample(1:nrow(data.bankruptcy),nrow(data.bankruptcy)*.7)

train <- data.bankruptcy[index,]

test <- data.bankruptcy[-index,]


##### Summary Stats #####


str(train)
summary(train) #no NA values

boxplot.stats(train$amount)

table(train$response)

##### EDA #####


#prep for correlation matrix

num <- sapply(train, is.numeric)

df_cor <- train[,num]

zv <- apply(df_cor, 2, function(x) length(unique(x)) <= 2)

sum(zv)
zv

df_cor <- df_cor[, !zv]

corr <- cor(df_cor,use = "pairwise.complete.obs")

highCorr <- findCorrelation(corr, 0.70)

length(highCorr) 

colnames(corr[, highCorr,drop = FALSE]) 

ggcorr(df_cor, method = c("complete", "pearson"), nbreaks = 10) #could also use pairwise, which is the default


##### Logistic Regression #####

model.logistic <- glm(response ~ . ,family = binomial, data = train)
summary(model.logistic)

# AIC

step.logistic <- step(model.logistic)

# BIC

step.logistic.BIC <- step(model.logistic, k = log(nrow(train)))


##### find cutoff probability with lowest cost #####

# define the searc grid from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
# result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd column
# stores the cost
result = cbind(searchgrid, NA)
# in the cost function, both r and pi are vectors, r=truth, pi=predicted
# probability
cost1 <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(train$response, predict(step.logistic, type = "response"))
}
plot(result, ylab = "Cost in Training Set") #.23 is cutoff with smallest cost

###### Confusion Matrix, ROC, AUC #####

# Confusion Matrix
prob.outsample <- predict(step.logistic, test, type = "response")

prob.outsample.binary <- as.numeric(predict(step.logistic, test, type = "response") > 0.23)

confusionMatrix(prob.outsample.binary,test$response)

# ROC
install.packages("ROCR")
library(ROCR)
pred <- prediction(prob.outsample, test$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

# AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values