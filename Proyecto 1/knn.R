###############################################################################
# Code for KNN starts here. You have to translate this code into C++ / Rcpp

my_knn_R = function(X, X0, y){
  # X data matrix with input attributes
  # y response variable values of instances in X  
  # X0 vector of input attributes for prediction
  
  nrows = nrow(X)
  ncols = ncol(X)
  
  # One of the instances is going to be the closest one:
  #   closest_distance: it is the distance , min_output
  closest_distance = 99999999
  closest_output = -1
  closest_neighbor = -1
  
  for(i in 1:nrows){
    
    distance = 0
    for(j in 1:ncols){
      difference = X[i,j]-X0[j]
      distance = distance + difference * difference
    }
    
    distance = sqrt(distance)
    
    if(distance < closest_distance){
      closest_distance = distance
      closest_output = y[i]
      closest_neighbor = i
    }
  }
  closest_output
}  

# Code for KNN ends here. 
##############################################################################



## Here, we test the function we just programmed ###

# X contains the inputs as a matrix of real numbers
data("iris")
# X contains the input attributes (excluding the class)
X <- iris[,-5]
# y contains the response variable (named medv, a numeric value)
y <- iris[,5]

# From dataframe to matrix
X <- as.matrix(X)
# From factor to integer
y <- as.integer(y)

# This is the point we want to predict

X0 <- c(5.80, 3.00, 4.35, 1.30)

# Using my_knn and FNN:knn to predict point X0
# Using the same number of neighbors, it should be similar (k=1)
my_knn_R(X, X0, y)
library(FNN)
FNN::knn(X, matrix(X0, nrow = 1), y, k=1)
