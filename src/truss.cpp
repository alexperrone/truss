//To compile:
// g++ truss.cpp -I /usr/local/lib/igraph/include/ -L /usr/local/lib/igraph/src/.libs -ligraph -o truss
//
//
// To run compiled code:
// LD_PRELOAD=/usr/local/lib/igraph/src/.libs/libigraph.so transitivity


#include <igraph.h>
#include <iostream>
#include <vector>
#include <set>
using namespace std;

void print_vector(igraph_vector_t *v, FILE *f);
void print_vector_int(igraph_vector_int_t *v, FILE *f);
void unpack(const igraph_vector_int_t& tri, igraph_vector_t& unpacked_tri);
void compute_support(const igraph_vector_t& eid, igraph_vector_int_t& support);
void trussness(const igraph_t& graph, igraph_vector_int_t& support,
               igraph_vector_int_t& truss);

int main() {

  igraph_vector_t v;
  igraph_vector_int_t triangles;
  igraph_t graph;

  igraph_real_t edges[] = { 1, 0, 2, 0, 3, 0, 1, 2, 1, 3, 3, 2, 3, 4, 4, 2 };
  igraph_vector_view(&v, edges, sizeof(edges)/sizeof(double));
  igraph_create(&graph, &v, 0, IGRAPH_UNDIRECTED);

  igraph_vector_int_init(&triangles, 0);  // needs a size, 0 is fine (will be resized)
  igraph_list_triangles(&graph, &triangles);

  // Print triangles.
  int i;
  cout << "triangles (as vertices): " << endl;
  print_vector_int(&triangles, stdout);

  // Unpack the triangles.
  igraph_vector_t unpacked_triangles;
  igraph_vector_init(&unpacked_triangles, 2 *
    igraph_vector_int_size(&triangles));

  unpack(triangles, unpacked_triangles);

  // Print unpacked triangles.
  cout << "unpacked triangles: " << endl;
  print_vector(&unpacked_triangles, stdout);

  // Get the edge ids of the unpacked triangles.
  igraph_vector_t eid;
  igraph_vector_init(&eid, igraph_vector_int_size(&triangles));
  igraph_get_eids(&graph, &eid, &unpacked_triangles, 0, 0, 1);

  // Print eids of the triangles.
  cout << "eids of triangles: " << endl;
  print_vector(&eid, stdout);

  // Compute the support of the edges.
  igraph_vector_int_t support;
  igraph_vector_int_init(&support, igraph_ecount(&graph));
  compute_support(eid, support);

  // Print support of edges.
  cout << "support of edges: " << endl;
  print_vector_int(&support, stdout);

  // Compute the truss of the edges.
  igraph_vector_int_t truss;
  igraph_vector_int_init(&truss, igraph_ecount(&graph));
  trussness(graph, support, truss);
  print_vector_int(&truss, stdout);

  // Clean up.
  igraph_vector_int_destroy(&triangles);
  igraph_vector_destroy(&unpacked_triangles);
  igraph_vector_destroy(&eid);
  igraph_vector_int_destroy(&support);
  igraph_vector_int_destroy(&truss);

  return 0;
}

void print_vector(igraph_vector_t *v, FILE *f) {
  long int i;
  for (i=0; i < igraph_vector_size(v); i++) {
    fprintf(f, " %li", (long int) VECTOR(*v)[i]);
  }
  fprintf(f, "\n");
}

void print_vector_int(igraph_vector_int_t *v, FILE *f) {
  long int i;
  for (i=0; i < igraph_vector_int_size(v); i++) {
    fprintf(f, " %li", (long int) VECTOR(*v)[i]);
  }
  fprintf(f, "\n");
}

void unpack(const igraph_vector_int_t& tri, igraph_vector_t& unpacked_tri) {
  int j;
  for (long int i = 0; i < igraph_vector_int_size(&tri); i = i + 3){
    j = i * 2;
    VECTOR(unpacked_tri)[j]   = VECTOR(unpacked_tri)[j+2] = VECTOR(tri)[i];
    VECTOR(unpacked_tri)[j+1] = VECTOR(unpacked_tri)[j+4] = VECTOR(tri)[i+1];
    VECTOR(unpacked_tri)[j+3] = VECTOR(unpacked_tri)[j+5] = VECTOR(tri)[i+2];
  }
}

void compute_support(const igraph_vector_t& eid, igraph_vector_int_t& support) {
  for (long int i = 0; i < igraph_vector_size(&eid); ++i){
    VECTOR(support)[(int) VECTOR(eid)[i]] += 1;
  }
  cout << endl;
}


/// Return the trussness for each edge.
void trussness(const igraph_t& graph, igraph_vector_int_t& support,
               igraph_vector_int_t& truss){

  // Get max possible value = max entry in support.
  int max = 0;
  for (long int i = 0; i < igraph_vector_int_size(&support); ++i) {
    int value = VECTOR(support)[i];
    if (max < value) { max = value; }
  }

  printf("max: %d\n", max);  // debug

  // The vector of sets. Each level of the vector is a set of edges initially
  // at that level of support, where support is # of triangles the edge is in.
  vector< set<long int> > sets(max + 1);

  // Add each edge to its appropriate level of support.
  for (long int i = 0; i < igraph_vector_int_size(&support); ++i) {
    sets[VECTOR(support)[i]].insert(i);  // insert edge i into its support level
  }

  // Record the trussness of edges at level 0.
  set<long int>::iterator it;
  for (it = sets[0].begin(); it != sets[0].end(); ++it){
    VECTOR(truss)[*it] = 0;
  }

  // Initialize number and set of completed edges.
  long int numCompleted = sets[0].size();
  set<long int> completed;  // set of completed edges
  cout << "numCompleted at start (edges at level 0): " << numCompleted << endl; //debug

  // Initialize variables needed below.
  igraph_integer_t fromVertex, toVertex, e1, e2;
  igraph_vector_t fromNeighbors, toNeighbors, q1, q2, commonNeighbors;
  igraph_vector_init(&fromNeighbors, 0);
  igraph_vector_init(&toNeighbors, 0);
  igraph_vector_init(&commonNeighbors, 0);

  // Move through the sets, one level at a time, starting at first level.
  for (int level = 1; level <= max; ++level){
    while (!sets[level].empty()){
      long int seed = *sets[level].begin();  // pull out the first edge
      sets[level].erase(seed);  // remove the first element

      // Find neighbors.
      igraph_edge(&graph, seed, &fromVertex, &toVertex);
      cout << "edge (seed): " << seed << ", vertices: " << fromVertex << ", " <<
        toVertex << endl;  // debug
      cout << "get neighbors" << endl;  // debug
      igraph_neighbors(&graph, &fromNeighbors, fromVertex, IGRAPH_ALL);
      igraph_neighbors(&graph, &toNeighbors, toVertex, IGRAPH_ALL);
      cout << "fromNeighbors:" << endl;
      print_vector(&fromNeighbors, stdout);
      cout << "toNeighbors:" << endl;
      print_vector(&toNeighbors, stdout);

      q1 = fromNeighbors;
      q2 = toNeighbors;

      cout << "set q1 and q2" << endl;  // debug
      if (igraph_vector_size(&q1) > igraph_vector_size(&q2)){
        // if we are here, fromNeighbors > toNeigbors, make q1 the smaller set
        q1 = toNeighbors;
        q2 = fromNeighbors;
      }

      cout << "q1:" << endl;  // debug
      print_vector(&q1, stdout);  // debug
      cout << "q2:" << endl;  // debug
      print_vector(&q2, stdout); // debug

      // Sort the neighbors.
      igraph_vector_sort(&q1);
      igraph_vector_sort(&q2);
      cout << "q1 (sorted):" << endl;  // debug
      print_vector(&q1, stdout);  // debug
      cout << "q2 (sorted):" << endl;  // debug
      print_vector(&q2, stdout);
      cout << "commonNeighbors (OLD -- before being set):" << endl;  // debug
      print_vector(&commonNeighbors, stdout); // debug
      igraph_vector_intersect_sorted(&q1, &q2, &commonNeighbors);
      cout << "commonNeighbors:" << endl;  // debug
      print_vector(&commonNeighbors, stdout); // debug
      cout << "done w common neighbors" << endl;

      for (int i = 0; i < igraph_vector_size(&commonNeighbors); i++){
        int n = VECTOR(commonNeighbors)[i];  // the common neighbor
        igraph_get_eid(&graph, &e1, fromVertex, n, IGRAPH_UNDIRECTED, 1);
        igraph_get_eid(&graph, &e2, toVertex, n, IGRAPH_UNDIRECTED, 1);

        printf("the eid from %d to %d is eid: %d\n", fromVertex, n, e1);  // debug
        bool e1_complete = (completed.find(e1) != completed.end());
        bool e2_complete = (completed.find(e2) != completed.end());
        if (!e1_complete && !e2_complete){
          // Demote this edge, if higher than current level.
          if (VECTOR(support)[e1] > level){
            VECTOR(support)[e1] -= 1;  // decrement the level
            int newLevel = VECTOR(support)[e1];
            sets[newLevel].insert(e1);
            sets[newLevel + 1].erase(e1);  // the old level
          }

          if (VECTOR(support)[e2] > level){
            VECTOR(support)[e2] -= 1;  // decrement the level
            int newLevel = VECTOR(support)[e2];
            sets[newLevel].insert(e2);
            sets[newLevel + 1].erase(e2);  // the old level
          }
        }
      }
      // Record this edge; its level is its trussness.
      VECTOR(truss)[seed] = level;
      completed.insert(seed);
      numCompleted++;
      cout << "=========================" << endl;  // debug
      igraph_vector_clear(&commonNeighbors);
    }  // end while
  }  // end for-loop over levels
}
