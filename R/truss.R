#' Cluster by k-truss
#'
#' Compute the k-truss of an undirected graph.
#'
#' @param graph An undirected graph (must be igraph).
#' @param k The level of support, requiring that every edge in subgraph is in
#' at least (k-2) triangles (J. Cohen 2008). \code{k} must be at least 3.
#' @param color_graph Logical to color the graph (perform truss membership).
#' @return A subgraph containing all k-trusses in the graph for a given level of support \code{k}.
#' @export
#' @keywords graphs
#' @references J. Cohen. Trusses: Cohesive subgraphs for social network analysis, 2008.
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
truss <- function(g, k, color_graph = FALSE){
  if (k < 3){
    stop("k must be at least 3")
  }
  g <- .validate_graph(g)

  edge_count <- igraph::ecount(g)
  if (edge_count == 0){
    return(g)
  }

  while (edge_count != 0){
    A <- igraph::get.adjacency(g)
    # The edge i-j occurs in number of triangles equal to scalar product of
    # row i and row j in the adjacency matrix A.
    triangle_matrix <- Matrix::tcrossprod(A)  # sparse matrix
    # Look up edges that are in at least (k-2) triangles.
    has_support <- (triangle_matrix >= (k-2))[igraph::get.edges(g, igraph::E(g))]
    # Trim to subgraph of supported edges.
    g <- igraph::subgraph.edges(g, which(has_support))
    # Re-compute number of edges in subgraph and see if it was trimmed.
    edge_count_new <- igraph::ecount(g)
    if (edge_count == edge_count_new) {
      break
    }
    edge_count <- edge_count_new
  }
  if (color_graph){
    g <- .color_graph(g)  # experimental
  }
  return(g)
}

#' Cluster by k-truss
#'
#' Compute maximal \code{k}-truss each edge belongs to.
#'
#' @param graph An undirected graph (must be igraph).
#' @return Vector indicating the maximal \code{k}-truss that each edge belongs to (NA if none).
#' @export
#' @keywords graphs
#' @references J. Cohen. Trusses: Cohesive subgraphs for social network analysis, 2008.
#' @examples
#'
#' require(igraph)
#' set.seed(1)
#' g <- sample_gnm(6, 9)
#' E(g)$truss <- truss_edge(g)
#'
#' # Plot original graph.
#' fixed_layout <- layout_with_lgl(g)
#' plot(g, layout = fixed_layout)  # original
#'
#' # Plot graph with truss clustering.
#' E(g)$weight <- 1
#' E(g)$color <- "gray"
#' E(g)[which(E(g)$truss <= 3)]$color <- "blue"
#' E(g)[which(E(g)$truss <= 3)]$weight <- 3
#' plot(g, layout = fixed_layout, edge.width = E(g)$weight)  # with truss
truss_edge <- function(g){
  g <- .validate_graph(g)

  edge_count <- igraph::ecount(g)
  if (edge_count == 0){
    return(g)
  }

  # Initialize eid_, which is a temporary variable needed to identify edges.
  igraph::E(g)$eid_ <- 1:igraph::ecount(g)

  truss_result <- rep(NA_integer_, igraph::ecount(g))  # to hold the truss result (maximal k)

  k <- 3  # initialize k, it will increment until the truss subgraph g is empty.
  while (igraph::ecount(g) != 0){
      g <- truss(g, k)
      truss_result[igraph::E(g)$eid_] <- k  # store k-truss
      k <- k + 1  # increment k
  }
  return(truss_result)
}


# Color the truss graph.
.color_graph <- function(g){
  igraph::E(g)$color <- NA
  color <- 1
  T <- matrix(igraph::triangles(g), nrow=3)
  for (i in seq(ncol(T))){
    edge_1 <- igraph::E(g, c(T[1, i], T[2, i]))
    edge_2 <- igraph::E(g, c(T[1, i], T[3, i]))
    edge_3 <- igraph::E(g, c(T[2, i], T[3, i]))
    if (!is.na(edge_1$color)){
      igraph::E(g, c(T[1, i], T[3, i]))$color <- edge_1$color
      igraph::E(g, c(T[2, i], T[3, i]))$color <- edge_1$color
    } else if (!is.na(edge_2$color)){
      igraph::E(g, c(T[1, i], T[2, i]))$color <- edge_2$color
      igraph::E(g, c(T[2, i], T[3, i]))$color <- edge_2$color
    } else if (!is.na(edge_3$color)){
      igraph::E(g, c(T[1, i], T[3, i]))$color <- edge_3$color
      igraph::E(g, c(T[2, i], T[3, i]))$color <- edge_3$color
    } else {
      igraph::E(g, c(T[1, i], T[2, i]))$color <- color
      igraph::E(g, c(T[1, i], T[3, i]))$color <- color
      igraph::E(g, c(T[2, i], T[3, i]))$color <- color
      color <- color + 1
    }
  }
  return(g)
}

# Validate the graph for truss (private function indicated by `.`).
.validate_graph <- function(g){
  if (!igraph::is_igraph(g)){
    stop("Not a graph object.")
  }
  if (igraph::is.directed(g)){
    stop("Graph must be undirected.")
  }
  if (igraph::any_multiple(g)){
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE,
                          edge.attr.comb = "first")
    warning("Duplicate edges removed.")
  }
  if (any(igraph::which_loop(g))){
    g <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE,
                          edge.attr.comb = "first")
    warning("Loops removed.")
  }
  return(g)
}

