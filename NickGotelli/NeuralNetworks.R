## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
X <- matrix(c(
  0,0,1,
  0,1,1,
  1,0,1,
  1,1,1
),
  ncol = 3,
  byrow = TRUE
)

# observed outcomes
y <- c(0, 1, 1, 0)

# print the data so we can take a quick look at it
cbind(X, y)

## ------------------------------------------------------------------------
# generate a random value between 0 and 1 for each
# element in X.  This will be used as our initial weights
# for layer 1
rand_vector <- runif(ncol(X) * nrow(X))

# convert above vector into a matrix
rand_matrix <- matrix(
  rand_vector,
  nrow = ncol(X),
  ncol = nrow(X),
  byrow = TRUE
)

# this list stores the state of our neural net as it is trained
my_nn <- list(
  # predictor variables
  input = X,
  # weights for layer 1
  weights1 = rand_matrix,
  # weights for layer 2
  weights2 = matrix(runif(4), ncol = 1),
  # actual observed
  y = y,
  # stores the predicted outcome
  output = matrix(
    rep(0, times = 4),
    ncol = 1
  )
)

## ------------------------------------------------------------------------
#' the activation function
sigmoid <- function(x) {
  1.0 / (1.0 + exp(-x))
}

#' the derivative of the activation function
sigmoid_derivative <- function(x) {
  x * (1.0 - x)
}

## ------------------------------------------------------------------------
loss_function <- function(nn) {
  sum((nn$y - nn$output) ^ 2)
}

## ------------------------------------------------------------------------
feedforward <- function(nn) {

  nn$layer1 <- sigmoid(nn$input %*% nn$weights1)
  nn$output <- sigmoid(nn$layer1 %*% nn$weights2)

  nn
}



## ------------------------------------------------------------------------
backprop <- function(nn) {

  # application of the chain rule to find derivative of the loss function with 
  # respect to weights2 and weights1
  d_weights2 <- (
    t(nn$layer1) %*%
    # `2 * (nn$y - nn$output)` is the derivative of the sigmoid loss function
    (2 * (nn$y - nn$output) *
    sigmoid_derivative(nn$output))
  )

  d_weights1 <- ( 2 * (nn$y - nn$output) * sigmoid_derivative(nn$output)) %*% 
    t(nn$weights2)
  d_weights1 <- d_weights1 * sigmoid_derivative(nn$layer1)
  d_weights1 <- t(nn$input) %*% d_weights1
  
  # update the weights using the derivative (slope) of the loss function
  nn$weights1 <- nn$weights1 + d_weights1
  nn$weights2 <- nn$weights2 + d_weights2

  nn
}

## ------------------------------------------------------------------------
# number of times to perform feedforward and backpropagation
n <- 1500

# data frame to store the results of the loss function.
# this data frame is used to produce the plot in the 
# next code chunk
loss_df <- data.frame(
  iteration = 1:n,
  loss = vector("numeric", length = n)
)

for (i in seq_len(1500)) {
  my_nn <- feedforward(my_nn)
  my_nn <- backprop(my_nn)

  # store the result of the loss function.  We will plot this later
  loss_df$loss[i] <- loss_function(my_nn)
}

# print the predicted outcome next to the actual outcome
data.frame(
  "Predicted" = round(my_nn$output, 3),
  "Actual" = y
)

## ------------------------------------------------------------------------
# plot the cost

ggplot(data = loss_df, aes(x = iteration, y = loss)) +
  geom_line()

