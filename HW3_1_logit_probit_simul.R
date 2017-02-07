#HW3 Problem 1

##### Simulation Study: Logistic/Probit Regression #####

# y|x ~ Binary(p), p = E(y|x),
# logit(pi) = -1.1 + 5*x1i -0.4*x2i
# x1i ~ Unif(0,1) 
# x2i ~ 1,0 for odd/even i's respectively

# set seed

set.seed(1984)

#sample size

n=500

# generate x1 and x2 data

x1 <- runif(n, min = 0, max = 1)

x2 <- rep(c(1,0), times= n/2)

#generate probabilities for logit and probit 

p.logit <- exp(-1.1+5*x1-.4*x2)/(1+exp(-1.1+5*x1-.4*x2))

p.probit <-pnorm(-1.1+5*x1-.4*x2, mean = 0, sd = 1, lower.tail = TRUE)

# generate y value for logit

y.logit<- rep(NA,length(p.logit))
output.logit <- rep(NA,length(p.logit))
for (i in 1:length(p.logit)){
  outcome.logit <-rbinom(1,1,p.logit[i])
  y.logit[i] <- outcome.logit
}

# generate y value for probit

y.probit<- rep(NA,length(p.probit))

for (i in 1:length(p.probit)){
  outcome.probit <-rbinom(1,1,p.probit[i])
 y.probit[i] <- outcome.probit
}

# create data fram
data.logit <- data.frame(y.logit,x1,x2)

data.probit <- data.frame(y.probit,x1,x2)

model.logit.logit <- glm(y.logit~.,family=binomial(link="logit"),data = data.logit)
model.logit.probit <- glm(y.logit~.,family=binomial(link="probit"),data = data.logit)

model.probit.probit <- glm(y.probit~.,family=binomial(link="probit"),data = data.probit)
model.probit.logit <- glm(y.probit~.,family=binomial(link="logit"),data = data.probit)

# model summaries from logit probabilities

summary(model.logit.logit)
summary(model.logit.probit)

AIC(model.logit.logit)
AIC(model.logit.probit)

BIC(model.logit.logit)
BIC(model.logit.probit)


# model summaries from probit probabilities

summary(model.probit.logit)
summary(model.probit.probit)

AIC(model.probit.logit)
AIC(model.probit.probit)

BIC(model.probit.logit)
BIC(model.probit.probit)
