library(class)
library(e1071)
library(nnet)
library(glmnet)
library(ggplot2)
library(magrittr)
library(randomForest)
library(pROC)

setwd("~/Dropbox/DP/Coursework/Fall2016/STAT275/")
#------------------------------------------------------------------------------#
# Use mean imputation for subjs with <= 1 NA, remove subjs with > 1 NA
spect_impute <- function(dat, indices) {
  row_na_counts <- apply(dat[, indices], c(1, 2), is.na) %>% apply(1, sum)
  dat_clean <- dat[row_na_counts <= 10, ]
  
  ROI_means <- apply(dat_clean[, indices], 2, mean, na.rm=T)
  
  dat_impute <- dat_clean
  for(ROI in indices) {
    dat_impute[ is.na(dat_impute[, ROI]), ROI] <- ROI_means[ROI - min(indices) + 1]
  }
  return(dat_impute)
}
#------------------------------------------------------------------------------#
# Import, clean, and organize ADHD data set
#------------------------------------------------------------------------------#
adhd_raw <- read.csv("data_ADHD.csv") %>% subset(group_name=="ADHD")
adhd_indices <- c(adhd_baseline <- 121:248, adhd_conc <- 249:376)
col_list <- c("Patient_ID", "BDI_Change", "group_name", "Pre_BDI", "Depressed", colnames(adhd_raw)[adhd_indices])
col_offset <- 5 # Number of cols before SPECT data

# Remove all rows that have duplicated Patient_IDs in the data set
# Select only defined columns from col_list
# Perform imputation and remove subjs with > 1 NA
# Remove any subjects with NA for BDI_Change
adhd_impute <-
  subset(adhd_raw, !(adhd_raw$Patient_ID %in% adhd_raw[duplicated(adhd_raw$Patient_ID), "Patient_ID"])) %>%
  subset(select=col_list) %>%
  spect_impute((col_offset + 1):(col_offset + length(adhd_indices))) %>%
  subset(!is.na(BDI_Change))

#------------------------------------------------------------------------------#
# Import, clean, and organize DEPRESSION data set
#------------------------------------------------------------------------------#
dep_raw <- read.csv("data_depression.csv") %>% subset(group_name=="Depression")
dep_indices <- c(dep_baseline <- 125:252, dep_conc <- 253:380)

# Same steps as used above for adhd_impute
dep_impute <-
  subset(dep_raw, !(dep_raw$Patient_ID %in% dep_raw[duplicated(dep_raw$Patient_ID), "Patient_ID"])) %>%
  subset(select=col_list) %>%
  spect_impute(col_offset:(col_offset + length(dep_indices))) %>%
  subset(!is.na(BDI_Change))

#------------------------------------------------------------------------------#
# Pool ADHD and DEP data sets and define response categories
#------------------------------------------------------------------------------#
# Merge Data and remove duplicates (subjs common to both data sets)
dat <- rbind(dep_impute, adhd_impute)
dat <- dat[!duplicated(dat$Patient_ID),]

# Redefine group names in adhd to match Depression diagnosis
# Define response category relative by median threshhold
dat$group_name <- with(dat, ifelse(Depressed==1, "Depression", "ADHD"))
dat$responder <- (dat$BDI_Change)<median(dat$BDI_Change)

# dep has all patients across both original data sets that have depression
# adhd has all adhd patients who do not have depression
#   (some subjs in dep have ADHD, no subjs in adhd have Depression)
dep <- subset(dat, group_name=="Depression")
adhd <- subset(dat, group_name=="ADHD")

#------------------------------------------------------------------------------#
# Plots and tables comparing ADHD and DEP subjs
#------------------------------------------------------------------------------#
ggplot(dat, aes(x=BDI_Change, fill=group_name)) + 
  geom_density(alpha=0.3)

ggplot(dat, aes(x=Pre_BDI, fill=group_name)) + 
  geom_density(alpha=0.3)

plot(dat$Pre_BDI, dat$BDI_Change, col=as.factor(dat$group_name))
boxplot(dat$BDI_Change ~ dat$group_name)

table(dat$group_name)
mean(dat$BDI_Change)
mean(subset(dat, group_name=="Depression", "BDI_Change")[[1]])
mean(subset(dat, group_name=="ADHD", "BDI_Change")[[1]])

t.test(dep$BDI_Change, adhd$BDI_Change)
t.test(dep$responder, adhd$responder)
table(dep$responder)
table(adhd$responder)

#------------------------------------------------------------------------------#
# Modeling
#------------------------------------------------------------------------------#
# Reduce data set to just group_name (response) and SPECT data (baseline and concentration)
dat_spect <- dat[, c("group_name", colnames(dat)[(col_offset + 1):(col_offset + length(adhd_indices))])]
dat_spect$group_name <- as.factor(dat$group_name)

# Create training and test data sets
set.seed(1234)
test_indices <- sample(1:nrow(dat_spect), 0.2*nrow(dat_spect))
test <- dat_spect[test_indices, ]
train <- dat_spect[-test_indices, ]

#------------------------------------------------------------------------------#
# Fit LASSO
fit_lasso <- cv.glmnet(x=as.matrix(train[, 2:ncol(train)]), y=train$group_name, family="binomial")
plot(fit_lasso)
plot(fit_lasso$cvm)
fit_lasso$nzero
fit_lasso$lambda.min

preds <- predict(fit_lasso, as.matrix(test[, 2:ncol(test)]))
roc_curve <- roc(dat_spect[test_indices,]$group_name, preds[,1])
plot(roc_curve)

#------------------------------------------------------------------------------#
# Random Forest
fit <- randomForest(group_name ~ ., data=dat_spect[-test_indices,], ntree=1000)
preds <- predict(fit, type="prob")
plot(roc(dat_spect[-test_indices,]$group_name, preds[,1]))
preds <- predict(fit, data=dat_spect[test_indices, ], type="prob")
roc_curve <- roc(dat_spect[test_indices,]$group_name, preds[,1])

#------------------------------------------------------------------------------#
# SVM
x = as.matrix(train[, 2:ncol(train)])
y = train$group_name
fit_svm <- svm(x, y, probability=TRUE)

summary(fit_svm)
preds <- predict(fit_svm, data=dat_spect[test_indices, ], decision.values=TRUE, probability=TRUE)
roc_curve <- roc(train$group_name, fit_svm$decision.values[,1])
plot(roc_curve)

#------------------------------------------------------------------------------#
# Neural Network
fit_nnet  <- nnet(group_name ~ ., data=train, size=2)
preds <- predict(fit_nnet, type="class")

fit_knn <- knn(train[, -1], test[, -1], train$group_name, k = 5)

#------------------------------------------------------------------------------#
# Tuning functions --- these can take a little while to run
tune_svm <- tune.svm(group_name ~ ., data=dat_spect[-test_indices,], probability=TRUE)
tune_rf <- tune.randomForest(x, y, ntree=c(500, 1000, 1500))
tune_knn <- tune.knn(train[, -1], train[, 1], k = seq(1, 100, 5))

plot(tune_svm)
plot(tune_rf)
plot(tune_knn)
