// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// my_3knn
double my_3knn(NumericMatrix X, NumericVector X0, NumericVector y);
RcppExport SEXP _knnpackage_my_3knn(SEXP XSEXP, SEXP X0SEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type X0(X0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(my_3knn(X, X0, y));
    return rcpp_result_gen;
END_RCPP
}
// my_3knn_invd
double my_3knn_invd(NumericMatrix X, NumericVector X0, NumericVector y);
RcppExport SEXP _knnpackage_my_3knn_invd(SEXP XSEXP, SEXP X0SEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type X0(X0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(my_3knn_invd(X, X0, y));
    return rcpp_result_gen;
END_RCPP
}
// my_knn_sourceCpp
double my_knn_sourceCpp(NumericMatrix X, NumericVector X0, NumericVector y);
RcppExport SEXP _knnpackage_my_knn_sourceCpp(SEXP XSEXP, SEXP X0SEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type X0(X0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(my_knn_sourceCpp(X, X0, y));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _knnpackage_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_knnpackage_my_3knn", (DL_FUNC) &_knnpackage_my_3knn, 3},
    {"_knnpackage_my_3knn_invd", (DL_FUNC) &_knnpackage_my_3knn_invd, 3},
    {"_knnpackage_my_knn_sourceCpp", (DL_FUNC) &_knnpackage_my_knn_sourceCpp, 3},
    {"_knnpackage_rcpp_hello_world", (DL_FUNC) &_knnpackage_rcpp_hello_world, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_knnpackage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
