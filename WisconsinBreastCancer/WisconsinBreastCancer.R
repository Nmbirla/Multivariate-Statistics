#######################################################################################################
## Author: Neha Birla
## Published: 11/1/2019
#######################################################################################################

# Read the file
setwd('N:\\umich Google Drive\\Education\\UMD\\Fall 2019\\IMSE 514\\Homeworks\\HW4\\R')
givenData <- read.csv('HW4-data.csv')


full_X=subset(givenData, select = -c(ID,Diagnosis)) # Extract predictors from complete data
Y<- as.factor(givenData$Diagnosis) # Extract target values and deifne target variables as factors

#######################################################################################################
# Spliting training and test data
#######################################################################################################
set.seed(213)   #  set seed to ensure you always have same random numbers generated
# Random sample indexes
train_index <- sample(1:nrow(full_X), 0.8 * nrow(full_X))
test_index <- setdiff(1:nrow(full_X), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- full_X[train_index, ]
y_train <- Y[train_index]

X_test <- full_X[test_index, ]
y_test <- Y[test_index]

#######################################################################################################
# General Linear Model (logit)
#######################################################################################################

# mdl<-glm(formula = Y~.,family = binomial, data = full_X)
# summary(mdl)


# Cov_mtx=cor(full_X)

# reduced_X = subset(givenData,select = c(RadiusM, TextureM, SmoothnessM, CompactnessM, ConcavePointsM, SymmetryM, FdimensionM, RadiusSE, TextureSE, SmoothnessSE, CompactnessSE, ConcavitySE, ConcavePointsSE, SymmetrySE, FdimensionSE, SmoothnessW, CompactnessW, ConcavityW, SymmetryW, FdimensionW))
# mdl2<-glm(formula=Y~., family=binomial,data=reduced_X)
# summary(mdl2)


#######################################################################################################
## Correlation between features studied using Matlab and some of the features eliminated
## Data for reduced number features used for backward selction
#######################################################################################################

reduced_X = subset(X_train,select = c(RadiusM, TextureM, SmoothnessM, CompactnessM, ConcavePointsM, SymmetryM, FdimensionM, RadiusSE, TextureSE, SmoothnessSE, CompactnessSE, ConcavitySE, ConcavePointsSE, SymmetrySE, FdimensionSE, SmoothnessW, CompactnessW, ConcavityW, SymmetryW, FdimensionW))
mdl2<-glm(formula=y_train~., family=binomial,data=reduced_X)
summary(mdl2)

control=glm.control(maxit=50)
mdl3 = step(mdl2,direction = "backward")
summary(mdl3)

# dummy_X = subset(givenData,select = c(RadiusM, TextureM, CompactnessM, ConcavePointsM, SymmetryM, RadiusSE, CompactnessSE, ConcavitySE, ConcavePointsSE, SymmetrySE, FdimensionSE, SmoothnessW, CompactnessW, ConcavityW, SymmetryW, FdimensionW))
# new.IVs <- dummy_X[74:81, ]
# Y_hat=predict(mdl3, newdata=new.IVs)
# 
# #Decision tree
# 
# for(i in 1:length(Y_hat)){
#   if(Y_hat[i]>0) print("M") else print("B")
# }

#######################################################################################################
## Testing the algorithm
#######################################################################################################


reduced_test_X = subset(X_test,select = c(RadiusM, TextureM, CompactnessM, ConcavePointsM, SymmetryM, RadiusSE, SmoothnessSE, CompactnessSE, ConcavitySE, ConcavePointsSE, FdimensionSE, SmoothnessW, CompactnessW, ConcavityW, SymmetryW, FdimensionW))

new.IVs <- reduced_test_X
Y_hat=predict(mdl3, newdata=new.IVs)


#######################################################################################################
## Decision tree 
## comapring given dianosis data with output target values from models 
## Counting number of diagnosis values not matching using j variable
#######################################################################################################
j=0
for(i in 1:length(Y_hat)){
  if(Y_hat[i]>0) flag="M" else flag="B"
  if(flag==y_test[i]) print("true") else{print("false"); j=j+1}
}
j

plot(Y_hat,mdl3$fitted.values)