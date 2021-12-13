log_regression <- function(X_train, Y_train, learning_rate, epochs, task = 1) {
  "
  This function implements the logistic regression algorithm and returns the final weights, bias and predicted values
  Inputs :
  X_train : The set of training features
  Y_train : The set of training labels
  learning_rate : the learning to be used while training
  epochs : number of iterations to be used for training
  task_46 = FALSE
  
  Outputs:
  weights : the final values of weights to be used for each feature for prediction
  bias : the final value of bias to be used while prediction
  sigmoid_val : the predicted value of each sample input
  "
  # Making sure that the x_train and y_train are in proper format for further processing
  x_train = as.matrix(X_train)
  y_train = (Y_train)
  
  # Initializing the weights and bias with random values with 0 or 
  # between -0.7 to 0.7 as per the task calling the function
  if (task == 1){
    weights = runif(ncol(x_train), min = 0, max = 0)
    bias  = 0
  }
  else if(task == 4 || task == 6 || task == 7){
    weights = runif(ncol(x_train), min = -0.7, max = 0.7)
    bias  = runif(1, min = -0.7, max = 0.7)
  }
  
  # Initializing a list to store values of cost function after each epoch to plot the cost vs iterations graph
  costs_list = 0
  
  # Loop for number of iterations
  for (i in 1:epochs){
    log_reg = weights %*% t(x_train) + bias
    sigmoid_val = 1/(1+exp(-log_reg))
    
    cost = -(sum((y_train * log(sigmoid_val)) + ((1-y_train) * log(1 - sigmoid_val))))
    costs_list[i] = cost
    
    deri_weights = ((sigmoid_val - y_train) %*% x_train)
    deri_bias = sum(sigmoid_val - y_train)
    weights = weights - (learning_rate*deri_weights)
    bias = bias - (learning_rate*deri_bias)
    
  }
  # plot(c(1:epochs),costs_list)
  
  # Returning the final weights, bias and the predicted values
  return_list = list("weights" = weights, "bias" = bias, "sigmoid_val" = sigmoid_val)
  return (return_list)
}
