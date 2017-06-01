#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector unpack(IntegerVector& tri) {
  int n_triangles = tri.size();
  IntegerVector v(2 * n_triangles);
  int i;
  int j;
  for (i = 0; i < n_triangles; i = i + 3){
    j = i * 2;
    v[j] = v[j+2] = tri[i];
    v[j+1] = v[j+4] = tri[i+1];
    v[j+3] = v[j+5] = tri[i+2];
  }
  return v;
}

// [[Rcpp::export]]
IntegerVector compute_support(IntegerVector& eid, int& n_edges) {
  IntegerVector spt(n_edges);
  for (int i = 0; i < eid.size(); ++i){
    ++spt[eid[i] - 1];  // edge IDs are 1-based in igraph, so shift left
  }
  return spt;
}


// R code.
/*** R
unpack(c(1, 2, 3, 66, 77, 88))
compute_support(c(1, 2, 1, 1, 1, 2, 3, 4), 4)
*/
