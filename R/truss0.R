#' Cluster by k-truss: Deprecated version.
#'
#' Compute the k-truss of an undirected graph (deprecated).
#'
#' @param graph An undirected graph (must be igraph).
#' @param k The level of support, requiring that every edge in subgraph is in
#' at least (k-2) triangles (J. Cohen 2008). \code{k} must be at least 3.
#' @return A subgraph containing all k-trusses in the graph for a given level of support \code{k}.
#' @export
#' @keywords graphs
#' @examples
#'
#' require(igraph)
#' set.seed(1)
#' g <- sample_gnm(6, 9)
#' E(g)$eid <- seq(ecount(g))
#' g_truss <- truss(g, k = 3)
#'
#' # Plot original graph.
#' fixed_layout <- layout_with_lgl(g)
#' plot(g, layout = fixed_layout)  # original
#'
#' # Plot graph with truss clustering.
#' E(g)$weight <- 1
#' E(g)$color <- "gray"
#' E(g)[E(g_truss)$eid]$color <- "blue"
#' E(g)[E(g_truss)$eid]$weight <- 3
#' plot(g, layout = fixed_layout, edge.width = E(g)$weight)  # with truss

truss0 <- function(g,k){
  empty.g <- graph.empty(0, directed=FALSE)

  # validate input igraph object.  must be undirected.
  if (igraph::is.directed(g)){
    print('error: graph object must be undirected.')
    return(empty.g)
  }

  edge_attr(g, "spt") <- 0
  num.tri <- length(count_triangles(g))
  num.tri.cur <- num.tri
  num.tri.new <- 0
  repeat {
    tr <- triangles(g)
    if (length(tr)==0){return(empty.g)}
    tri.edges <- tapply(tr, rep(1:(length(tr)/3), each = 3), function(x) E(g,path=c(x,x[1])))
    for (i in 1:ecount(g)){
      spt <- sum(unlist(tri.edges) == E(g)[i])
      E(g)$spt[i] <- spt
    }
    g <- subgraph.edges(g,E(g)[spt >= (k-2)])
    num.tri.new <- length(count_triangles(g))
    if (num.tri.cur == num.tri.new) {break}
    num.tri.cur <- num.tri.new
  }
  return(g)
}
