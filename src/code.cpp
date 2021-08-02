// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;


//' Accumulate vector based on conditional threshold
//'
//' The vector will accumulate each pair of adjacent elements, similar to \code{\link[base]{Reduce}} with the accumulate parameter.
//' If the summation of the two elements is greater than the threshold, the returned value of that cycle is zero. Primary use is within
//' \code{\link{assign_episode}} function to provide a speed boost in the looping procedure.
//'
//' @param x Numeric vector.
//' @param threshold Integer value.
//' @return Numeric vector.
//' @export
// [[Rcpp::export]]
std::vector<int>  accumulate_threshold(NumericVector x, int threshold) {
  // Initialize
  std::vector<int> out_vec;
  out_vec.reserve(x.length());

  // Iterate with conditional lambda
  std::accumulate(x.cbegin(),
                  x.cend(),
                  0,
                  [&out_vec,threshold](double x, double y){
                    int new_val = (x+y > threshold) ? 0 : x+y;
                    out_vec.push_back(new_val);
                    return new_val;
                  });

  return out_vec;
}
