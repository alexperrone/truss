#' Cluster by k-truss
#'
#' Compute the k-truss of an undirected graph.
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

truss <- function(g, k){
  if (k < 3){
    stop("k must be at least 3")
  }
  g <- .validate_graph(g)

  # Subset to only those nodes with degree at least (k-1)?
  # g <- igraph::induced_subgraph(g, vids = igraph::V(g)[igraph::degree(g) >= (k - 1)])

  # Subset to only those nodes that occur in at least (k-2) triangles?
  # g <- igraph::induced_subgraph(g, vids = igraph::V(g)[igraph::count_triangles(g) >= (k - 2)])

  edge_count <- igraph::ecount(g)

  if (edge_count == 0){
    return(igraph::make_empty_graph(n = 0, directed = FALSE))
  }

  while (edge_count != 0){
    A <- igraph::get.adjacency(g)
    # The edge i-j occurs in number of triangles equal to scalar product of
    # row i and row j in the adjacency matrix A.
    triangle_matrix <- Matrix::tcrossprod(A)  # sparse matrix
    # Each edge needs to be in at least (k-2) triangles.
    support_matrix <- as.matrix(triangle_matrix >= (k - 2))
    # Look up support for each edge.
    has_support <- support_matrix[igraph::ends(g, igraph::E(g))]
    # Trim to subgraph of supported edges.
    g <- igraph::subgraph.edges(g, which(has_support))
    # Re-compute number of edges in subgraph and see if it was trimmed.
    edge_count_new <- igraph::ecount(g)
    if (edge_count == edge_count_new) {
      break
    }
    # do only if ecount(g) is non-zero??
    # color_matrix <- .color_graph(support_matrix)  # add edge colorings
    edge_count <- edge_count_new
  }
  # print(system.time(g <- .color_graph(g)))
  return(g)
}


# TODO: put in truss():
.color_graph <- function(g){
  E(g)$color <- NA
  color <- 1
  T <- matrix(triangles(g), nrow=3)
  for (i in seq(ncol(T))){
    edge_1 <- E(g, c(T[1, i], T[2, i]))
    edge_2 <- E(g, c(T[1, i], T[3, i]))
    edge_3 <- E(g, c(T[2, i], T[3, i]))
    if (!is.na(edge_1$color)){
      E(g, c(T[1, i], T[3, i]))$color <- edge_1$color
      E(g, c(T[2, i], T[3, i]))$color <- edge_1$color
    } else if (!is.na(edge_2$color)){
      E(g, c(T[1, i], T[2, i]))$color <- edge_2$color
      E(g, c(T[2, i], T[3, i]))$color <- edge_2$color
    } else if (!is.na(edge_3$color)){
      E(g, c(T[1, i], T[3, i]))$color <- edge_3$color
      E(g, c(T[2, i], T[3, i]))$color <- edge_3$color
    } else {
      E(g, c(T[1, i], T[2, i]))$color <- color
      E(g, c(T[1, i], T[3, i]))$color <- color
      E(g, c(T[2, i], T[3, i]))$color <- color
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

