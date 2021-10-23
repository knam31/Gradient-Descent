
#function to compute sigmoid
sigmoid <- function(x){
  return (1/(1+exp(-x)))
}

#forward propagation function for initial test
forward_prop <- function(w,x,b){
  y_prob <- sigmoid(rowSums(x%*%w)+b)
  return (y_prob)
}

#function to compute Mean square error
MSE <- function(y_pred,y_train){
  return (mean((y_pred-y_train)^2))
}

#Task:1-function to perform gradient descent 
gradient_descent <- function(x_train,y_train,x_test,y_test,learning_rate = 0.1,n = 100) {
  set.seed(seed)
  
  #initializing weights and bias with values in range -0.7 to 0.7
  w <- runif(ncol(x_train),min=-0.7,max=0.7)
  set.seed(seed)
  b <- runif(1,min=-0.7,max=0.7)
  
  #variable to store test MSE values for each training step 
  MSE_test <- rep(0, n)
  
  
  #training steps
  for(i in 1:n){
    y_prob <- forward_prop(w,x_train,b)
    A <- (y_prob-y_train)
    w <- w + learning_rate*-2*colMeans(x_train * A)
    b <- b + learning_rate*-2*mean(A)
    
    y_test_pred <- testing(test_data,w,b)
    MSE_test[i] <- MSE(y_test_pred,y_test) 
    
  }
  
  param <- list("weight"=w,"bias"=b)
  
  #MSE vs iterations plot
  plot(MSE_test,type='l',xlab="Training Steps",ylab="Test MSE",main="Test MSE Boxplot")
  
  
  return(param)
  
  
}

#function to predict labels
testing <- function(x_test,w,b){
  y_prob <- forward_prop(w,x_test,b)
  
  #classifying into 1 or 0 
  y_prob[y_prob>=0.5] <- 1
  y_prob[y_prob<0.5] <- 0
  return(y_prob)
}

#seed value set as MMDD of birthdate
seed <- 731

library(ISLR)
attach(Auto)

#Task:2
#creating variable high which is label
high <- rep(0,nrow(Auto))
high[mpg>=23] <- 1

#creating dummy variables for origin
dummyorigin_2 <- rep(0,nrow(Auto))
dummyorigin_3 <- rep(0,nrow(Auto))

dummyorigin_2[origin==2] <-1
dummyorigin_3[origin==3] <-1

 
Auto.sample <- data.frame(horsepower,weight,year,dummyorigin_2,dummyorigin_3)
Auto.sample <- scale(Auto.sample)
set.seed(seed)

#Task:3-splitting into training and test data
train <- sample(1:length(high),length(high)/2)
train_data <- Auto.sample[train,]
test_data <- Auto.sample[-train,]


#Task: 4-logisitc regression with different values of learning rate and stopping rule
learning_rates <-  c(0.0001,0.001,0.01,0.1)
number_of_iter <- c(500,1000,2000,100)

c <-sprintf("Learning rate\tStopping Rule\tTest MSE\tTrain MSE\n")
cat(c)
for (i in 1:length(learning_rates)){

param = gradient_descent(train_data,high[train],test_data,high[-train],learning_rates[i],number_of_iter[i])

y_pred_test <- testing(test_data, param$weight, param$bias)
y_pred_train <- testing(train_data, param$weight, param$bias)

#Test and train MSE's for different values of lr and stopping rule
MSE_test <- MSE(y_pred_test,high[-train])
MSE_train <- MSE(y_pred_train,high[train])

c <-sprintf("%.4f\t\t%f\t%f\t%f\n",learning_rates[i],number_of_iter[i],MSE_test,MSE_train)
cat(c)
}

#Task:6-logisitc regression with fixed values of learning rate and stopping rule
param = gradient_descent(train_data,high[train],test_data,high[-train])

y_pred <- testing(test_data, param$weight, param$bias)

MSE_t <- MSE(y_pred,high[-train])

#Accuracy
me <- 1-MSE_t





