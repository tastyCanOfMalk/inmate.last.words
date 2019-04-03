if(!require(rpart)) install.packages("rpart")
library(rpart)
if(!require(rpart.plot)) install.packages("rpart.plot")
library(rpart.plot) # rpart.plot

xPart <- xCom %>% 
  as_tibble() %>% 
  select(Race,
         EducationLevel,
         PreviousCrime,
         NumberVictim,
         FemaleVictim,
         MaleVictim,
         AWR,
         AgeExec,
         Codefendants,
         Served) %>% 
  mutate(Race = as.factor(Race)) %>% 
  mutate(EducationLevel = as.factor(EducationLevel)) %>%
  mutate(PreviousCrime = as.factor(PreviousCrime))
  # mutate(NumberVictim = as.factor(NumberVictim)) %>% 
  # mutate(FemaleVictim = as.factor(FemaleVictim)) %>% 
  # mutate(MaleVictim = as.factor(MaleVictim)) %>% 
  # mutate(AWR = as.factor(AWR)) %>% 
  # mutate(AgeExec = as.factor(AgeExec)) %>% 
  # mutate(Codefendants = as.factor(Codefendants)) %>% 
  # mutate(Served = as.factor(Served))

glimpse(xPart)

# create test and train groups
set.seed(1)
samp  <- sample(1:nrow(xPart), nrow(xPart)*.7)
train <- xPart[ samp,]
test  <- xPart[-samp,]

glimpse(xPart)

# create model
fit <- rpart(Race~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(EducationLevel~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(PreviousCrime~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(NumberVictim~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(FemaleVictim~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(MaleVictim~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(AWR~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(AgeExec~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(Codefendants~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)

# create model
fit <- rpart(Served~.,train)
# plot model
rpart.plot(fit,type=2,extra=100)








# predict on test set
y_hat <- predict(fit,test[-10])

# simply round to get T/F values
y_hat <- y_hat %>% 
  as_tibble() %>% 
  round(0) %>% 
  mutate(X11.yhat = ifelse(`TRUE` == 1,TRUE,FALSE)) %>% 
  select(X11.yhat)

# confusion matrix
cm <- table(unlist(y_hat),test$X11)  

# accuracy
acc <- sum(diag(cm))/sum(cm)
acc # 95%
