#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]



double my_3knn(NumericMatrix X, NumericVector X0, NumericVector y){
  // X data matrix with input attributes
  // y response variable values of instances in X  
  // X0 vector of input attributes for prediction
  
  int nrows=X.nrow();
  int ncols=X.ncol();
  
  // One of the instances is going to be the closest one:
  //closest_distance: it is the distance , min_output
  
  double closest_distance = 99999999;
  int closest_output = -1;
  int closest_neighbor = -1;
  
  //initialize vector to save the i where we have already selected the closest neighbor
  //so that in next iteration we find the second closest neighbor
  NumericVector closest_neighbors;
  closest_neighbors.push_back(-1);
  NumericVector closest_outputs;
  NumericVector closest_distances;
  
  //find 3 nearest neighbors
  for(int k=0; k<3;k++){
    
    for(int i=0; i<nrows; i++){
      
      //check that i is not inside an already found closed neighbor
      //find points to end of array if element not inside vector
      
      if(std::find(closest_neighbors.begin(), closest_neighbors.end(), i) == closest_neighbors.end()){
    
        double distance = 0;
    
        for(int j=0; j<ncols; j++){
          double difference = X(i,j)-X0[j];
          distance += pow(difference,2);
        }
    
        distance = sqrt(distance);
    
        if(distance < closest_distance){
          closest_distance = distance;
          closest_output = y[i];
          closest_neighbor = i;
        }
      }
    }
    //append new closest neighbor, output and distance to the vector
    closest_neighbors.push_back(closest_neighbor);
    closest_outputs.push_back(closest_output);
    closest_distances.push_back(closest_distance);
  }
  
  //now find the "vote" on the 3 first neighbors
  //find the most repeated element in the vector of outputs (in this case just two is enough)
  //if none of them are repeated(i.e three different, return the closest one (the first))
  
  if(closest_outputs[0]!=closest_outputs[1] & closest_outputs[0]!=closest_outputs[2] && closest_outputs[2]!=closest_outputs[1]){
    
    return(closest_outputs[0]);
    
  }
  else{
    for(int o=0; o<3; o++){
      for(int w=0; w<3; w++){
        if(closest_outputs[o]==closest_outputs[w] & o!=w){
          return(closest_outputs[o]);
        }
      }
    }
  }
  
}
