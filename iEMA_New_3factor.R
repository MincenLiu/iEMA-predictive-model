library(softImpute)
setwd("/Users/mincen/Desktop/Honors Thesis/iEMA-predictive-model")
###########################################################
### Step 1: prepare iEMA object  
###########################################################
## ref:  IEMA_initial_v3.R 
## Input: id.coln,items,const.coln,time.colns,RawOrganizedData in "ShiffmanRawOrganized.RData"
## output: iEMAObj, a list with relevant information  

## rm(list = ls.str())
load(file = "ShiffmanRawOrganized.RData")
## source("IEMA_functions.R")

###########################################################
### Step 2: operations on iEMAObj, step by step 
###########################################################
## First, we fit a linear mixed model for each outcome to get gamma.
library(lme4)
library(arm) # convenience functions for regression in R
library(dplyr)
head(RawOrganizedData)
set.seed(123)
# Rename columns where names are "B_URGE*" and "B_CRAVING*"
names(RawOrganizedData)[names(RawOrganizedData) == "B_URGE*"] <- "B_URGE_star"
names(RawOrganizedData)[names(RawOrganizedData) == "B_CRAVING*"] <- "B_CRAVING_star"
names(RawOrganizedData)

# Exclude observations with missing covariates, otherwise the model will give outputs NA
RawOrganizedData$missing_cov = is.na(RawOrganizedData$GENDER) + is.na(RawOrganizedData$AGE) + is.na(RawOrganizedData$DOW) + 
  is.na(RawOrganizedData$AVECIGS) +  is.na(RawOrganizedData$ADDICTED) +  is.na(RawOrganizedData$MINTOFIR) +  is.na(RawOrganizedData$RefDay) +
  is.na(RawOrganizedData$TIME) + is.na(RawOrganizedData$SUBJECT)
# hasmissing <- RawOrganizedData[(RawOrganizedData$missing_cov > 0), ]
# hasmissingcov <- select(hasmissing, GENDER, AGE, DOW, AVECIGS, ADDICTED, MINTOFIR, RefDay, TIME, SUBJECT) 
RawOrganizedData_full <- RawOrganizedData[(RawOrganizedData$missing_cov == 0), ] # Remove observations with missing covariates
split <- sample(c(rep(0, 0.8 * nrow(RawOrganizedData_full)), rep(1, 0.2 * nrow(RawOrganizedData_full))))
RawOrganizedData_complete <- RawOrganizedData_full[split == 0, ]
test <- RawOrganizedData_full[split == 1, ]

m1 <- lmer(B_URGE_star ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
# display(m1)
# gamma1 <- fixef(m1)
RawOrganizedData_complete$B_URGE_star_hat <- predict(m1, na.action = na.exclude)
E1 <- RawOrganizedData_complete$B_URGE_star - RawOrganizedData_complete$B_URGE_star_hat
# summary(E1)
gamma1 <- summary(m1)$coefficients[, 1]
rand1 <- ranef(m1)

m2 <- lmer(B_CRAVING_star ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$B_CRAVING_star_hat <- predict(m2, na.action = na.exclude)
E2 <- RawOrganizedData_complete$B_CRAVING_star - RawOrganizedData_complete$B_CRAVING_star_hat
gamma2 <- summary(m2)$coefficients[, 1]
rand2 <- ranef(m2)

m3 <- lmer(RESTLESS ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$RESTLESS_hat <- predict(m3, na.action = na.exclude)
E3 <- RawOrganizedData_complete$RESTLESS - RawOrganizedData_complete$RESTLESS_hat
gamma3 <- summary(m3)$coefficients[, 1]
rand3 <- ranef(m3)

m4 <- lmer(TIRED ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$TIRED_hat <- predict(m4, na.action = na.exclude)
E4 <-RawOrganizedData_complete$TIRED - RawOrganizedData_complete$TIRED_hat
gamma4 <- summary(m4)$coefficients[, 1]
rand4 <- ranef(m4)

m5 <- lmer(HAPPY ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$HAPPY_hat <- predict(m5, na.action = na.exclude)
E5 <- RawOrganizedData_complete$HAPPY - RawOrganizedData_complete$HAPPY_hat
gamma5 <- summary(m5)$coefficients[, 1]
rand5 <- ranef(m5)

m6 <- lmer(IRRITABLE ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$IRRITABLE_hat <- predict(m6, na.action = na.exclude)
E6 <- RawOrganizedData_complete$IRRITABLE - RawOrganizedData_complete$IRRITABLE_hat
gamma6 <- summary(m6)$coefficients[, 1]
rand6 <- ranef(m6)

m7 <- lmer(SPACEY ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$SPACEY_hat <- predict(m7, na.action = na.exclude)
E7 <- RawOrganizedData_complete$SPACEY - RawOrganizedData_complete$SPACEY_hat
gamma7 <- summary(m7)$coefficients[, 1]
rand7 <- ranef(m7)

m8 <- lmer(MISERABLE ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$MISERABLE_hat <- predict(m8, na.action = na.exclude)
E8 <- RawOrganizedData_complete$MISERABLE - RawOrganizedData_complete$MISERABLE_hat
gamma8 <- summary(m8)$coefficients[, 1]
rand8 <- ranef(m8)

m9 <- lmer(SLEEPY ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$SLEEPY_hat <- predict(m9, na.action = na.exclude)
E9 <- RawOrganizedData_complete$SLEEPY - RawOrganizedData_complete$SLEEPY_hat
gamma9 <- summary(m9)$coefficients[, 1]
rand9 <- ranef(m9)

m10 <- lmer(TENSE ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$TENSE_hat <- predict(m10, na.action = na.exclude)
E10 <- RawOrganizedData_complete$TENSE - RawOrganizedData_complete$TENSE_hat
gamma10 <- summary(m10)$coefficients[, 1]
rand10 <- ranef(m10)

m11 <- lmer(CONTENTED ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$CONTENTED_hat <- predict(m11, na.action = na.exclude)
E11 <- RawOrganizedData_complete$CONTENTED - RawOrganizedData_complete$CONTENTED_hat
gamma11 <- summary(m11)$coefficients[, 1]
rand11 <- ranef(m11)

m12 <- lmer(FRUSTR_ANG ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$FRUSTR_ANG_hat <- predict(m12, na.action = na.exclude)
E12 <- RawOrganizedData_complete$FRUSTR_ANG - RawOrganizedData_complete$FRUSTR_ANG_hat
gamma12 <- summary(m12)$coefficients[, 1]
rand12 <- ranef(m12)

m13 <- lmer(ENERGETIC ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$ENERGETIC_hat <- predict(m13, na.action = na.exclude)
E13 <- RawOrganizedData_complete$ENERGETIC - RawOrganizedData_complete$ENERGETIC_hat
gamma13 <- summary(m13)$coefficients[, 1]
rand13 <- ranef(m13)

m14 <- lmer(SAD ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$SAD_hat <- predict(m14, na.action = na.exclude)
E14 <- RawOrganizedData_complete$SAD - RawOrganizedData_complete$SAD_hat
gamma14 <- summary(m14)$coefficients[, 1]
rand14 <- ranef(m14)

m15 <- lmer(HARD_CONCE ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$HARD_CONCE_hat <- predict(m15, na.action = na.exclude)
E15 <- RawOrganizedData_complete$HARD_CONCE - RawOrganizedData_complete$HARD_CONCE_hat
gamma15 <- summary(m15)$coefficients[, 1]
rand15 <- ranef(m15)

m16 <- lmer(HUNGRY ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$HUNGRY_hat <- predict(m16, na.action = na.exclude)
E16 <- RawOrganizedData_complete$HUNGRY - RawOrganizedData_complete$HUNGRY_hat
gamma16 <- summary(m16)$coefficients[, 1]
rand16 <- ranef(m16)

m17 <- lmer(B_AFFECT ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$B_AFFECT_hat <- predict(m17, na.action = na.exclude)
E17 <- RawOrganizedData_complete$B_AFFECT - RawOrganizedData_complete$B_AFFECT_hat
gamma17 <- summary(m17)$coefficients[, 1]
rand17 <- ranef(m17)

m18 <- lmer(B_AROUSE ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
RawOrganizedData_complete$B_AROUSE_hat <- predict(m18, na.action = na.exclude)
E18 <- RawOrganizedData_complete$B_AROUSE - RawOrganizedData_complete$B_AROUSE_hat
gamma18 <- summary(m18)$coefficients[, 1]
rand18 <- ranef(m18)

# Save all Es into an N-by-J matrix
E <- cbind(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18)
gamma <- cbind(gamma1, gamma2, gamma3, gamma4, gamma5, gamma6, gamma7, gamma8, gamma9, gamma10, gamma11, gamma12, gamma13, gamma14, gamma15, gamma16, gamma17, gamma18)

# Factor analysis model
library(psych)
library(GPArotation)
library(corrplot)
## Create the correlation matrix from Es
# lowerCor calls cor with use='pairwise', method=‘pearson’ as default values and returns (invisibly) the full correlation matrix and displays
# the lower off diagonal matrix.
E_cor <- cor(E[complete.cases(E),]) # Remove missing when construsting the correlation matrix
corrplot(E_cor, method="number") # Look at the correlations among our variables
factors_data <- fa(E_cor, 3)

# Graph factor loading matrices
fa.diagram(factors_data)

# Get F (factor scores) matrix
my.scores <- factor.scores(E, factors_data)
f <- my.scores$scores

# Linear mixed model to find betas
MMR1 <- lmer(f[, 1] ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
beta1 <- summary(MMR1)$coefficients[, 1]
randMMR1 <- ranef(MMR1) # 0

MMR3 <- lmer(f[, 2] ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
beta3 <- summary(MMR3)$coefficients[, 1]
randMMR3 <- ranef(MMR3) # 0

MMR2 <- lmer(f[, 3] ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME + (1 | SUBJECT), data = RawOrganizedData_complete)
beta2 <- summary(MMR2)$coefficients[, 1]
randMMR2 <- ranef(MMR2) # 0

# Prediction using the training dataset
train_input <- data.frame(1, RawOrganizedData_complete$GENDER, RawOrganizedData_complete$AGE, RawOrganizedData_complete$DOW, RawOrganizedData_complete$AVECIGS, RawOrganizedData_complete$ADDICTED, RawOrganizedData_complete$MINTOFIR, RawOrganizedData_complete$RefDay, RawOrganizedData_complete$TIME, RawOrganizedData_complete$SUBJECT)

train_input = merge(train_input, data.frame(rand1)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand1"

train_input = merge(train_input, data.frame(rand2)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand2"

train_input = merge(train_input, data.frame(rand3)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand3"

train_input = merge(train_input, data.frame(rand4)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand4"

train_input = merge(train_input, data.frame(rand5)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand5"

train_input = merge(train_input, data.frame(rand6)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand6"

train_input = merge(train_input, data.frame(rand7)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand7"

train_input = merge(train_input, data.frame(rand8)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand8"

train_input = merge(train_input, data.frame(rand9)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand9"

train_input = merge(train_input, data.frame(rand10)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand10"

train_input = merge(train_input, data.frame(rand11)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand11"

train_input = merge(train_input, data.frame(rand12)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand12"

train_input = merge(train_input, data.frame(rand13)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand13"

train_input = merge(train_input, data.frame(rand14)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand14"

train_input = merge(train_input, data.frame(rand15)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand15"

train_input = merge(train_input, data.frame(rand16)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand16"

train_input = merge(train_input, data.frame(rand17)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand17"

train_input = merge(train_input, data.frame(rand18)[,c(3,4)], all.x=TRUE, by.x="RawOrganizedData_complete.SUBJECT", by.y="grp")
names(train_input)[names(train_input) == "condval"] <- "rand18"

idmat18 <- diag(18)
gamma_new <- rbind(gamma, idmat18)

z_gamma_train <- as.matrix(train_input[ , -1]) %*% gamma_new

train_input2 <- data.frame(1, RawOrganizedData_complete$GENDER, RawOrganizedData_complete$AGE, RawOrganizedData_complete$DOW, RawOrganizedData_complete$AVECIGS, RawOrganizedData_complete$ADDICTED, RawOrganizedData_complete$MINTOFIR, RawOrganizedData_complete$RefDay, RawOrganizedData_complete$TIME)
F1_hat_train <- as.matrix(train_input2) %*% as.matrix(beta1)
F3_hat_train <- as.matrix(train_input2) %*% as.matrix(beta3)
F2_hat_train <- as.matrix(train_input2) %*% as.matrix(beta2)

F_matrix_train <- cbind(F1_hat_train, F3_hat_train, F2_hat_train)

f_lambda_train <- F_matrix_train %*% t(factors_data$loadings)

Y_hat_train = z_gamma_train + f_lambda_train

Y_real_train <- select(RawOrganizedData_complete, B_URGE_star, B_CRAVING_star, RESTLESS, TIRED, HAPPY, IRRITABLE, SPACEY, MISERABLE, SLEEPY, TENSE, CONTENTED, FRUSTR_ANG, ENERGETIC, SAD, HARD_CONCE, HUNGRY, B_AFFECT, B_AROUSE)
pred_error_train <- Y_real_train - Y_hat_train

pred_error1_train <- mean(pred_error_train$B_URGE_star^2, na.rm = TRUE)
pred_error2_train <- mean(pred_error_train$B_CRAVING_star^2, na.rm = TRUE)
pred_error3_train <- mean(pred_error_train$RESTLESS^2, na.rm = TRUE)
pred_error4_train <- mean(pred_error_train$TIRED^2, na.rm = TRUE)
pred_error5_train <- mean(pred_error_train$HAPPY^2, na.rm = TRUE)
pred_error6_train <- mean(pred_error_train$IRRITABLE^2, na.rm = TRUE)
pred_error7_train <- mean(pred_error_train$SPACEY^2, na.rm = TRUE)
pred_error8_train <- mean(pred_error_train$MISERABLE^2, na.rm = TRUE)
pred_error9_train <- mean(pred_error_train$SLEEPY^2, na.rm = TRUE)
pred_error10_train <- mean(pred_error_train$TENSE^2, na.rm = TRUE)
pred_error11_train <- mean(pred_error_train$CONTENTED^2, na.rm = TRUE)
pred_error12_train <- mean(pred_error_train$FRUSTR_ANG^2, na.rm = TRUE)
pred_error13_train <- mean(pred_error_train$ENERGETIC^2, na.rm = TRUE)
pred_error14_train <- mean(pred_error_train$SAD^2, na.rm = TRUE)
pred_error15_train <- mean(pred_error_train$HARD_CONCE^2, na.rm = TRUE)
pred_error16_train <- mean(pred_error_train$HUNGRY^2, na.rm = TRUE)
pred_error17_train <- mean(pred_error_train$B_AFFECT^2, na.rm = TRUE)
pred_error18_train <- mean(pred_error_train$B_AROUSE^2, na.rm = TRUE)

hist(pred_error_train$B_URGE_star)
quantile(pred_error_train$B_URGE_star, probs=c(.995,1), na.rm = TRUE)


pred_error_lmm_train <- Y_real_train - z_gamma_train
p1_train <- mean(pred_error_lmm_train$B_URGE_star^2, na.rm = TRUE)

slr1 <- lm(B_URGE_star ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME, data = RawOrganizedData_complete)
s1 <- summary(slr1)$coefficients[, 1]
slr1_pred_train <- as.matrix(train_input2) %*% as.matrix(s1)
slr1_pred_e_train <- Y_real_train$B_URGE_star - slr1_pred_train
slr1_mse_train <- mean(slr1_pred_e_train^2, na.rm = TRUE)

quantile(slr1_pred_e_train, probs=c(.95,1), na.rm = TRUE)
hist(slr1_pred_e_train)

# Prediction using the testing dataset
test_input <- data.frame(1, test$GENDER, test$AGE, test$DOW, test$AVECIGS, test$ADDICTED, test$MINTOFIR, test$RefDay, test$TIME, test$SUBJECT)

test_input = merge(test_input, data.frame(rand1)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand1"

test_input = merge(test_input, data.frame(rand2)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand2"

test_input = merge(test_input, data.frame(rand3)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand3"

test_input = merge(test_input, data.frame(rand4)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand4"

test_input = merge(test_input, data.frame(rand5)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand5"

test_input = merge(test_input, data.frame(rand6)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand6"

test_input = merge(test_input, data.frame(rand7)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand7"

test_input = merge(test_input, data.frame(rand8)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand8"

test_input = merge(test_input, data.frame(rand9)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand9"

test_input = merge(test_input, data.frame(rand10)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand10"

test_input = merge(test_input, data.frame(rand11)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand11"

test_input = merge(test_input, data.frame(rand12)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand12"

test_input = merge(test_input, data.frame(rand13)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand13"

test_input = merge(test_input, data.frame(rand14)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand14"

test_input = merge(test_input, data.frame(rand15)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand15"

test_input = merge(test_input, data.frame(rand16)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand16"

test_input = merge(test_input, data.frame(rand17)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand17"

test_input = merge(test_input, data.frame(rand18)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
names(test_input)[names(test_input) == "condval"] <- "rand18"

z_gamma <- as.matrix(test_input[ , -1]) %*% gamma_new


test_input2 <- data.frame(1, test$GENDER, test$AGE, test$DOW, test$AVECIGS, test$ADDICTED, test$MINTOFIR, test$RefDay, test$TIME)
F1_hat <- as.matrix(test_input2) %*% as.matrix(beta1)
F3_hat <- as.matrix(test_input2) %*% as.matrix(beta3)
F2_hat <- as.matrix(test_input2) %*% as.matrix(beta2)

F_matrix <- cbind(F1_hat, F3_hat, F2_hat)

f_lambda <- F_matrix %*% t(factors_data$loadings)

# test_input2 = merge(test_input2, data.frame(randMMR1)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR1"
# 
# test_input2 = merge(test_input2, data.frame(randMMR2)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR2"
# 
# test_input2 = merge(test_input2, data.frame(randMMR3)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR3"
# 
# test_input2 = merge(test_input2, data.frame(randMMR6)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR6"
# 
# test_input2 = merge(test_input2, data.frame(randMMR4)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR4"
# 
# test_input2 = merge(test_input2, data.frame(randMMR5)[,c(3,4)], all.x=TRUE, by.x="test.SUBJECT", by.y="grp")
# names(test_input2)[names(test_input2) == "condval"] <- "randMMR5"


# Y_hat = z_gamma + f_lambda
Y_hat = z_gamma + f_lambda

# Get mean squared error for each outcome
Y_real <- select(test, B_URGE_star, B_CRAVING_star, RESTLESS, TIRED, HAPPY, IRRITABLE, SPACEY, MISERABLE, SLEEPY, TENSE, CONTENTED, FRUSTR_ANG, ENERGETIC, SAD, HARD_CONCE, HUNGRY, B_AFFECT, B_AROUSE)
pred_error <- Y_real - Y_hat

pred_error1 <- mean(pred_error$B_URGE_star^2, na.rm = TRUE)
pred_error2 <- mean(pred_error$B_CRAVING_star^2, na.rm = TRUE)
pred_error3 <- mean(pred_error$RESTLESS^2, na.rm = TRUE)
pred_error4 <- mean(pred_error$TIRED^2, na.rm = TRUE)
pred_error5 <- mean(pred_error$HAPPY^2, na.rm = TRUE)
pred_error6 <- mean(pred_error$IRRITABLE^2, na.rm = TRUE)
pred_error7 <- mean(pred_error$SPACEY^2, na.rm = TRUE)
pred_error8 <- mean(pred_error$MISERABLE^2, na.rm = TRUE)
pred_error9 <- mean(pred_error$SLEEPY^2, na.rm = TRUE)
pred_error10 <- mean(pred_error$TENSE^2, na.rm = TRUE)
pred_error11 <- mean(pred_error$CONTENTED^2, na.rm = TRUE)
pred_error12 <- mean(pred_error$FRUSTR_ANG^2, na.rm = TRUE)
pred_error13 <- mean(pred_error$ENERGETIC^2, na.rm = TRUE)
pred_error14 <- mean(pred_error$SAD^2, na.rm = TRUE)
pred_error15 <- mean(pred_error$HARD_CONCE^2, na.rm = TRUE)
pred_error16 <- mean(pred_error$HUNGRY^2, na.rm = TRUE)
pred_error17 <- mean(pred_error$B_AFFECT^2, na.rm = TRUE)
pred_error18 <- mean(pred_error$B_AROUSE^2, na.rm = TRUE)

# If only use the first step:
pred_error_lmm <- Y_real - z_gamma
p1 <- mean(pred_error_lmm$B_URGE_star^2, na.rm = TRUE)

# For comparison purpose: fit a simple linear regression
slr1 <- lm(B_URGE_star ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME, data = RawOrganizedData_complete)
s1 <- summary(slr1)$coefficients[, 1]
slr1_pred <- as.matrix(test_input2) %*% as.matrix(s1)
slr1_pred_e <- Y_real$B_URGE_star - slr1_pred
slr1_mse <- mean(slr1_pred_e^2, na.rm = TRUE)

slr2 <- lm(B_CRAVING_star ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME, data = RawOrganizedData_complete)
s2 <- summary(slr2)$coefficients[, 1]
slr2_pred <- as.matrix(test_input2) %*% as.matrix(s2)
slr2_pred_e <- Y_real$B_CRAVING_star - slr2_pred
slr2_mse <- mean(slr2_pred_e^2, na.rm = TRUE)

slr14 <- lm(SAD ~ GENDER + AGE + DOW + AVECIGS + ADDICTED + MINTOFIR + RefDay + TIME, data = RawOrganizedData_complete)
s14 <- summary(slr14)$coefficients[, 1]
slr14_pred <- as.matrix(test_input2) %*% as.matrix(s14)
slr14_pred_e <- Y_real$SAD - slr14_pred
slr14_mse <- mean(slr14_pred_e^2, na.rm = TRUE)





