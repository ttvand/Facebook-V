// #include <Rcpp.h>
  #include <RcppArmadillo.h>
  // [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector fastOrder(arma::vec x) {
  return(Rcpp::as<Rcpp::NumericVector>(wrap(arma::sort_index( x )+1)) );
}