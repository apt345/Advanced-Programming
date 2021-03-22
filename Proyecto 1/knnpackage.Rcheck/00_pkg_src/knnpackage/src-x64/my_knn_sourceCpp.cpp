#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]

double my_knn_sourceCpp(NumericMatrix X, NumericVector X0, NumericVector y){
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
  
  for(int i=0; i<nrows; i++){
    
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
  return(closest_output);
}
