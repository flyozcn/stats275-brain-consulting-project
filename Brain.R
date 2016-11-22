# only look at depression patients
Depression <- Depression[Depression$GROUP_ID==2,]

# there are missing values in life quality score and raw scores are difficult to interpret
summary(Depression$Pre_QOLI)
summary(Depression$POST_QOLI)

# depression score
summary(Depression$Pre_BDI)
summary(Depression$POST_BDI)

library(ggplot2)
# most patients are moderately or severely depressed before treatment
qplot(Depression$Pre_BDI)
# most patients are mildly depressed after
qplot(Depression$POST_BDI)
# improvements are mostly positive
qplot(Depression$Pre_BDI-Depression$POST_BDI)

# baseline brain regions
summary(Depression[,125:252])
# missing values
NAs <- apply(Depression[,125:252], 1, function(x) sum(is.na(x)))
# about 120 patients have 1 or 2 missing values but about 90 patients have more than 100 missing values
qplot(NAs[NAs>0])

# remove patients and do simple mean imputation
Depression <- Depression[NAs<100,]
summary(Depression[,125:252])

Depression[,'T_Baseline_Cerebellum_10_L'][is.na(Depression[,'T_Baseline_Cerebellum_10_L'])] <- mean(Depression[,'T_Baseline_Cerebellum_10_L'], na.rm=TRUE)
Depression[,'T_Baseline_Cerebellum_10_R'][is.na(Depression[,'T_Baseline_Cerebellum_10_R'])] <- mean(Depression[,'T_Baseline_Cerebellum_10_R'], na.rm=TRUE)
Depression[,'T_Baseline_Cerebellum_3_L'][is.na(Depression[,'T_Baseline_Cerebellum_3_L'])] <- mean(Depression[,'T_Baseline_Cerebellum_3_L'], na.rm=TRUE)
Depression[,'T_Baseline_Vermis_1_2'][is.na(Depression[,'T_Baseline_Vermis_1_2'])] <- mean(Depression[,'T_Baseline_Vermis_1_2'], na.rm=TRUE)
Depression[,'T_Baseline_Vermis_10'][is.na(Depression[,'T_Baseline_Vermis_10'])] <- mean(Depression[,'T_Baseline_Vermis_10'], na.rm=TRUE)

# same for concentration brain regions
summary(Depression[,253:380])
NAs <- apply(Depression[,253:380], 1, function(x) sum(is.na(x)))
qplot(NAs[NAs>0])
Depression <- Depression[NAs<100,]

Depression[,'T_Concentration_Cerebellum_10_L'][is.na(Depression[,'T_Concentration_Cerebellum_10_L'])] <- mean(Depression[,'T_Concentration_Cerebellum_10_L'], na.rm=TRUE)
Depression[,'T_Concentration_Cerebellum_3_L'][is.na(Depression[,'T_Concentration_Cerebellum_3_L'])] <- mean(Depression[,'T_Concentration_Cerebellum_3_L'], na.rm=TRUE)
Depression[,'T_Concentration_Vermis_1_2'][is.na(Depression[,'T_Concentration_Vermis_1_2'])] <- mean(Depression[,'T_Concentration_Vermis_1_2'], na.rm=TRUE)
Depression[,'T_Concentration_Vermis_10'][is.na(Depression[,'T_Concentration_Vermis_10'])] <- mean(Depression[,'T_Concentration_Vermis_10'], na.rm=TRUE)
Depression[,'T_Concentration_Vermis_9'][is.na(Depression[,'T_Concentration_Vermis_9'])] <- mean(Depression[,'T_Concentration_Vermis_9'], na.rm=TRUE)

############################## Focus on outcome ##############################

# how is non-responder determined
summary(Depression$Responder)
summary(Depression$BDI_Change[Depression$Responder=='Responder'])
summary(Depression$BDI_Change[Depression$Responder!='Responder'])

# with respect to pre BDI
qplot(Depression$Pre_BDI,(Depression$Pre_BDI-Depression$POST_BDI))
ggplot(data=Depression,aes(x=Pre_BDI,y=Pre_BDI-POST_BDI,colour=Treatment_Response_Depression)) + geom_point()

# create new outcome measure
outcome <- (Depression$Pre_BDI-Depression$POST_BDI)>0.5*Depression$Pre_BDI
temp <- data.frame(Pre_BDI=Depression$Pre_BDI,POST_BDI=Depression$POST_BDI,outcome)
ggplot(data=temp,aes(x=Pre_BDI,y=Pre_BDI-POST_BDI,colour=outcome)) + geom_point()

############################## Raw data visualization ##############################

# visualize brain region scores for each patient
i <- 1
b <- as.numeric(baseline[i,])
x <- seq(1,128,1)
qplot(x,b,geom='line',xlab='Region ID',ylab='T Score',main=paste('Patient',i))

# create outcome arbitrarily for now
outcome <- Depression$Responder=='Responder'
summary(outcome)

# stack columns for plotting
stackregion <- function(regions, outcome) {
  n <- length(outcome)
  score <- rep(NA, n*8)
  name <- rep(NA, n*8)
  responder <- rep(NA, n*8)
  patient <- rep(NA, n*8)
  for (i in 1:8) {
    score[((i-1)*n+1):((i-1)*n+n)] <- as.numeric(regions[,i])
    name[((i-1)*n+1):((i-1)*n+n)] <- colnames(regions)[i]
    responder[((i-1)*n+1):((i-1)*n+n)] <- outcome
    patient[((i-1)*n+1):((i-1)*n+n)] <- seq(1, n, 1)
  }
  stacked <- data.frame(score,name,responder,patient)
  return(stacked)
}

# baseline boxplots
for (i in 1:16) {
  regions <- stackregion(Depression[,(125+(i-1)*8):(132+(i-1)*8)],outcome)
  p <- ggplot(data=regions,aes(x=as.factor(name),y=score,fill=responder)) + 
    geom_boxplot() + labs(x=paste('Regions',(i-1)*8+1,'to',(i-1)*8+8))
  print(p)
}

# baseline brain waves
for (i in 1:16) {
  regions <- stackregion(Depression[,(125+(i-1)*8):(132+(i-1)*8)],outcome)
  p <- ggplot(data=regions,aes(x=as.factor(name),y=score,group=patient,colour=responder)) + 
    geom_line(alpha=0.2) + labs(x=paste('Regions',(i-1)*8+1,'to',(i-1)*8+8))
  print(p)
}

# concentration boxplots
for (i in 1:16) {
  regions <- stackregion(Depression[,(253+(i-1)*8):(260+(i-1)*8)],outcome)
  p <- ggplot(data=regions,aes(x=as.factor(name),y=score,fill=responder)) + 
    geom_boxplot() + labs(x=paste('Regions',(i-1)*8+1,'to',(i-1)*8+8))
  print(p)
}

# concentration brain waves
for (i in 1:16) {
  regions <- stackregion(Depression[,(253+(i-1)*8):(260+(i-1)*8)],outcome)
  p <- ggplot(data=regions,aes(x=as.factor(name),y=score,group=patient,colour=responder)) + 
    geom_line(alpha=0.2) + labs(x=paste('Regions',(i-1)*8+1,'to',(i-1)*8+8))
  print(p)
}

############################## Naive non bayes modeling ##############################

baseline <- Depression[,125:252]
concentration <- Depression[,253:380]

brain <- cbind(baseline, concentration, pre_BDI=Depression$Pre_BDI, outcome)

# split training and test data
indices <- sample(1:802,802)
test <- brain[indices[1:200],]
train <- brain[indices[201:802],]

library(e1071)
library(xgboost)
dat <- cbind(outcome,baseline,Depression$Pre_BDI)
indices <- sample(1:828,800)
dat <- dat[indices,]
accuracy1 <- 0
accuracy2 <- 0
for (i in 1:5) {
  current <- seq(1+(i-1)*160,160+(i-1)*160,1)
  train <- dat[-current,]
  test <- dat[current,]
  fit1 <- svm(as.factor(outcome)~.,data=train)
  pred1 <- predict(fit1,test[,-1],type='class')
  accuracy1 <- accuracy1+sum(test$outcome==pred1)
  fit2 <- xgboost(data=as.matrix(train[,-1]),label=train$outcome,nrounds=10,max.depth=5,
                 objective='binary:logistic',verbose=0)
  pred2 <- predict(fit2, as.matrix(test[,-1]))
  accuracy2 <- accuracy2+sum(test$outcome==(pred2>0.5))
}
accuracy1/800
accuracy2/800

# logistic regression 
fit <- glm(as.factor(outcome)~.,data=train,family='binomial')
summary(fit)
pred <- predict(fit,test[,-258],type="response")
sum(test$outcome==(pred>0.5))
# overfitting check
pred <- predict(fit,train[,-258],type="response")
sum(train$outcome==(pred>0.5))

# support vector machine does slightly better
library(e1071)
fit <- svm(as.factor(outcome)~.,data=train,kernel='radial')
pred <- predict(fit,test[,-258],type='class')
sum(test$outcome==pred)
# overfitting check
pred <- predict(fit,train[,-258],type='class')
sum(train$outcome==pred)

# boosting does not work so well
library(xgboost)
fit <- xgboost(data=as.matrix(train[,-258]),label=train$outcome,nrounds=10,max.depth=5,
               objective='binary:logistic')
pred <- predict(fit, as.matrix(test[,-258]))
sum(test$outcome==(pred>0.5))
# overfitting check
pred <- predict(fit,as.matrix(train[,-258]))
sum(train$outcome==(pred>0.5))

# linear regression for sanity check
brain$outcome <- Depression$POST_BDI
fit <- lm(outcome~.,data=brain)
summary(fit)

############################## Dimensionlarity reduction ##############################

# principle component analysis is virtually useless
brain <- Depression[,125:380]
fit <- princomp(baseline, cor=TRUE)
plot(fit,type="lines") 

pca1 <- fit$scores[,1]
pca2 <- fit$scores[,2]
pca <- data.frame(pca1, pca2, outcome)
ggplot(data=pca, aes(x=pca1, y=pca2, colour=outcome)) + geom_point()

# baseline only
baseline <- Depression[,125:252]
fit1 <- princomp(baseline, cor=TRUE)
plot(fit1,type="lines") 

pca1B <- fit1$scores[,1]
pca2B <- fit1$scores[,2]
pcaB <- data.frame(pca1B, pca2B, outcome)
ggplot(data=pcaB, aes(x=pca1B, y=pca2B, colour=outcome)) + geom_point()

# concentration only
concentration <- Depression[,253:380]
fit2 <- princomp(concentration, cor=TRUE)
plot(fit2,type="lines") 

pca1C <- fit2$scores[,1]
pca2C <- fit2$scores[,2]
pcaC <- data.frame(pca1C, pca2C, outcome)
ggplot(data=pcaC, aes(x=pca1C, y=pca2C, colour=outcome)) + geom_point()

library(tsne)
tsne_data <- tsne(baseline, k=2, max_iter=100, epoch=20)
embedding <- data.frame(d1=tsne_data[,1], d2=tsne_data[,2], outcome)
ggplot(data=embedding, aes(x=d1, y=d2, colour=outcome)) + geom_point()

############################## Diagnosis ##############################

diagnosis <- Depression[,57:119]
diagnosis <- diagnosis[,-c(12, 30, 41)]

# remove columns of all 0's or 1's
# Dementia_Alzheimers_Type, Vascular_dementia, Communication_Disorder, Mental_Retardation, Motor_Skill_Disorder
# Other_disorders_infancy_childhood_adolesc, Factitious_disorders, MoodDisorder, Depressed, Personality_disorders
# Personality_Disorder_NOS, Paraphilias, Parasomnia, Caffeine_related_disorders, Hallucinogen_related_disorders
diagnosis <- diagnosis[,!apply(diagnosis==0,2,all)]
diagnosis <- diagnosis[,!apply(diagnosis==1,2,all)]

medical <- cbind(diagnosis, Depression$Pre_BDI, outcome)

# split training and test data
indices <- sample(1:802,802)
test <- medical[indices[1:200],]
train <- medical[indices[201:802],]

# logistic regression 
fit <- glm(as.factor(outcome)~.,data=train,family='binomial')
summary(fit)
pred <- predict(fit,test[,-47],type="response")
sum(test$outcome==(pred>0.5))
# overfitting check
pred <- predict(fit,train[,-47],type="response")
sum(train$outcome==(pred>0.5))

############################## Penalized regression ##############################

library(glmnet)
# throw everything into design matrix
X <- as.matrix(cbind(baseline, Pre_BDI=Depression$Pre_BDI))

# split training and test data
indices <- sample(1:828,828)
Xtest <- X[indices[1:160],]
ytest <- outcome[indices[1:160]]
Xtrain <- X[indices[161:828],]
ytrain <- outcome[indices[161:828]]

fit <- glmnet(Xtrain, as.factor(ytrain), family='binomial', standardize=FALSE)
plot(fit, xvar = "lambda", label = TRUE)
pred <- predict(fit, newx=Xtest, type='response', s=0.2)
sum(ytest==(pred>0.5))

coef(fit, s=0.1)

cvfit = cv.glmnet(X, outcome, family = "binomial", type.measure = "auc")
plot(cvfit)

############################## Manual features ##############################
outcome <- no_na_depression$depression.outcome
diagnosis <- no_na_depression[34:95]
brain <- t_baseline[2:124]
feature1 <- brain$T_Baseline_Cerebellum_4_5_L*(diagnosis$Bipolar-1)
feature2 <- brain$T_Baseline_Cerebellum_4_5_R*(diagnosis$Bipolar-1)
feature3 <- brain$T_Baseline_Cerebellum_4_5_L*(diagnosis$Sleep_Disorders-1)
feature4 <- brain$T_Baseline_Cerebellum_4_5_R*(diagnosis$Sleep_Disorders-1)
feature5 <- brain$T_Baseline_Frontal_Inf_Tri_L*(diagnosis$SubstanceAbuseDisorder-1)
feature6 <- brain$T_Baseline_Frontal_Inf_Tri_R*(diagnosis$SubstanceAbuseDisorder-1)
feature7 <- brain$T_Baseline_Cerebellum_6_L*(diagnosis$TemporalDysfunction-1)
feature8 <- brain$T_Baseline_Cerebellum_6_R*(diagnosis$TemporalDysfunction-1)
features <- cbind(feature1, feature2, feature3, feature4, feature5, feature6, feature7, feature8)

dat <- data.frame(outcome, features, brain)
# split training and test data
indices <- sample(1:954,954)
test <- dat[indices[1:200],]
train <- dat[indices[201:954],]

# logistic regression 
fit <- glm(as.factor(outcome)~.,data=train,family='binomial')
summary(fit)
pred <- predict(fit,test[,-1],type="response")
sum(test$outcome==(pred>0.5))
# overfitting check
pred <- predict(fit,train[,-1],type="response")
sum(train$outcome==(pred>0.5))