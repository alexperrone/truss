//To compile:
// gcc truss.c -I /usr/local/lib/igraph/include/ -L /usr/local/lib/igraph/src/.libs -ligraph -o truss
//
// To run compiled code:
// LD_PRELOAD=/usr/local/lib/igraph/src/.libs/libigraph.so transitivity


#include <igraph.h>
#include <iostream>
#include <vector>
#include <set>
using namespace std;


void unpack(const igraph_vector_int_t& tri, igraph_vector_t& unpacked_tri);


int main() {

  igraph_vector_t v;
  igraph_vector_int_t triangles;
  igraph_t graph;

  igraph_real_t edges[] = { 0, 1, 0, 2, 1, 2, 2, 3, 2, 4, 3, 4 };
  igraph_vector_view(&v, edges, sizeof(edges)/sizeof(double));
  igraph_create(&graph, &v, 0, IGRAPH_UNDIRECTED);

  igraph_vector_int_init(&triangles, 0);  // needs a size, 0 is fine (will be resized)
  igraph_list_triangles(&graph, &triangles);


  // Print triangles.
  int i;
  cout << "triangles: ";
  for (i = 0; i < igraph_vector_int_size(&triangles); i++){
    cout << VECTOR(triangles)[i] << ", ";
  }
  cout << endl;

  // Unpack the triangles.
  igraph_vector_t unpacked_triangles;
  igraph_vector_init(&unpacked_triangles, 2 *
    igraph_vector_int_size(&triangles));

  unpack(triangles, unpacked_triangles);

  // Print unpacked triangles.
  cout << "unpacked triangles: ";
  for (int k = 0; k < igraph_vector_size(&unpacked_triangles); k++){
    cout << VECTOR(unpacked_triangles)[k] << ", ";
  }
  cout << endl;

  // Get the edge ids of the unpacked triangles.
  igraph_vector_t eid;
  igraph_vector_init(&eid, igraph_vector_int_size(&triangles));
  igraph_get_eids(&graph, &eid, &unpacked_triangles, 0, 0, 1);

  // Print eids of the triangles.
  cout << "eids of triangles: ";
  for (int m = 0; m < igraph_vector_size(&eid); m++){
    cout << VECTOR(eid)[m] << ", ";
  }
  cout << endl;

  // Clean up.
  igraph_vector_int_destroy(&triangles);
  igraph_vector_destroy(&unpacked_triangles);
  igraph_vector_destroy(&eid);
  return 0;
}


void unpack(const igraph_vector_int_t& tri, igraph_vector_t& unpacked_tri) {
  int j;
  for (int i = 0; i < igraph_vector_int_size(&tri); i = i + 3){
    j = i * 2;
    VECTOR(unpacked_tri)[j]   = VECTOR(unpacked_tri)[j+2] = VECTOR(tri)[i];
    VECTOR(unpacked_tri)[j+1] = VECTOR(unpacked_tri)[j+4] = VECTOR(tri)[i+1];
    VECTOR(unpacked_tri)[j+3] = VECTOR(unpacked_tri)[j+5] = VECTOR(tri)[i+2];
  }
}

// // [[Rcpp::export]]
// IntegerVector compute_support(IntegerVector& eid, int& n_edges) {
//   IntegerVector spt(n_edges);
//   for (int i = 0; i < eid.size(); ++i){
//     ++spt[eid[i] - 1];  // edge IDs are 1-based in igraph, so shift left
//   }
//   return spt;
// }
