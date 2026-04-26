#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
arma::mat har_agg(arma::vec RM, arma::vec periods, int iNperiods){
  int iT = RM.size();
  arma::mat mHARData(iT, iNperiods);
  mHARData.fill(NA_REAL);

  // Trailing mean per period column via running sum: O(iT) per column
  // instead of O(iT * period) when each output element resums the window.
  for(int i = 0; i<iNperiods; i++){
    int p = periods(i);
    if(p > iT) continue;
    double s = sum(RM(arma::span(0, p-1)));
    mHARData(p-1, i) = s/p;
    for(int j = p+1; j<=iT; j++){
      s += RM(j-1) - RM(j-p-1);
      mHARData(j-1, i) = s/p;
    }
  }

  return(mHARData);
}
