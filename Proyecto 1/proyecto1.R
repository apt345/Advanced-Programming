#proyecto 1
library(Rcpp)
library(microbenchmark)
setwd("C:/Users/arpri/OneDrive/Escritorio/libros/master/Segundo Semicuatrimestre/ProgramacionAvanzada/Proyecto 1/")
sourceCpp("proyecto1.cpp")

##R version of the code
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
my_knn_sourceCpp(X, X0, y)

library(FNN)
FNN::knn(X, matrix(X0, nrow = 1), y, k=1)

#they give the same results (2)

#Check speed

microbenchmark(my_knn_sourceCpp(X, X0, y), times=100)#wayyy faster 4.32 micros

microbenchmark(FNN::knn(X, matrix(X0, nrow = 1), y, k=1), times=100) #394 micros

#############################################################################################

#part 2: I modify the code in the package for it to work with k=3 neighbors
#adding a new function: my_3knn

library(knnpackage)

my_3knn(X, X0, y)#2, perfect
#check results with FNN

FNN::knn(X, matrix(X0, nrow = 1), y, k=3)

##R version of the code

my_3knn_R=function(X, X0, y){
  
  nrows=nrow(X)
  ncols=ncol(X)
  
  
  closest_distance = 99999999
  closest_output = -1
  closest_neighbor = -1
  
  
  closest_neighbors=c(-1)
  closest_outputs=numeric(length=3)
  closest_distances=numeric(length=3)
  
 
  for(k in (1:3)){
    
    for(i in (1:nrows)){
      
      
      if(!(i %in% closest_neighbors)){
        
        distance = 0
        
        for(j in (1:ncols)){
          difference = X[i,j]-X0[j]
          distance =distance + difference*difference
        }
        
        distance = sqrt(distance)
        
        if(distance < closest_distance){
          closest_distance = distance
          closest_output = y[i]
          closest_neighbor = i
        }
      }
    }
    closest_neighbors[k]= closest_neighbor
    closest_outputs[k]= closest_output
    closest_distances[k]= closest_distance
  }
  
  
  
  if(closest_outputs[1]!=closest_outputs[2] && closest_outputs[1]!=closest_outputs[3] && closest_outputs[3]!=closest_outputs[2]){
    
    return(closest_outputs[1])
    
  }
  else{
    for(o in (1:3)){
      for(w in (1:3)){
        if(closest_outputs[o]==closest_outputs[w] && o!=w){
          return(closest_outputs[o])
        }
      }
    }
  }
  
}

my_3knn_R(X, X0, y) #2, perfect

#compare speed with microbenchmark

microbenchmark(my_3knn(X, X0, y), times=100) #10.5 micros

microbenchmark(my_3knn_R(X, X0, y), times=100) #3.13 miliseconds, slow

microbenchmark(FNN::knn(X, matrix(X0, nrow = 1), y, k=3), times=100) #314 micros

###############################################################################

#part 3, work with inverse of distance

my_3knn_invd(X, X0, y)
#also yields 2 as result