context("truss equality")

test_that("test invalid k", {
  g <- igraph::make_empty_graph(0, directed = FALSE)
  expect_error(truss(g, -123), "k must be at least 3")
})

test_that("test no truss", {
  g <- igraph::make_empty_graph(0, directed = FALSE)
  expect_true(igraph::identical_graphs(g, truss(g, k = 3)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 4)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 5)))
})

test_that("test truss equality for 7-clique", {
  g <- igraph::make_full_graph(n = 7)
  expect_true(igraph::identical_graphs(g, truss(g, k = 3)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 4)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 5)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 6)))
  expect_true(igraph::identical_graphs(g, truss(g, k = 7)))
  g_8_truss <- truss(g, k = 8)
  expect_equal(length(igraph::E(g_8_truss)), 0)
  expect_equal(length(igraph::V(g_8_truss)), 0)
  g_9_truss <- truss(g, k = 9)
  expect_equal(length(igraph::E(g_9_truss)), 0)
  expect_equal(length(igraph::V(g_9_truss)), 0)
})

test_that("test truss equality for dolphins dataset", {
  nodes <- read.csv("dolphins_nodes.csv")
  edges <- read.csv("dolphins_edges.csv")
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  expect_warning(g_5_truss <- truss(g, k = 5), "Duplicate edges removed.")
  expect_identical(igraph::as_data_frame(g_5_truss),
                   structure(list(from = c("6", "6", "6", "6", "9", "9", "9", "13",
                                           "13", "17", "18", "18", "18", "18",
                                           "18", "21", "21", "21", "24",
                                           "24", "24", "29", "29", "45"),
                                  to = c("9", "13", "17", "57", "13", "17",
                                         "57", "17", "57", "57", "21", "24",
                                         "29", "45", "51", "29", "45", "51",
                                         "29", "45", "51", "45", "51", "51")),
                             .Names = c("from", "to"),
                             class = "data.frame", row.names = c(NA, 24L)))
  expect_warning(g_6_truss <- truss(g, k = 6), "Duplicate edges removed.")
  expect_equal(length(igraph::E(g_6_truss)), 0)
  expect_equal(length(igraph::V(g_6_truss)), 0)
})

test_that("test truss equality for Truss dataset", {
  nodes <- read.csv("Truss_nodes.csv")
  edges <- read.csv("Truss_edges.csv")
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  g_3_truss <- truss(g, k = 3)
  expect_identical(igraph::as_data_frame(g_3_truss),
                   structure(list(
                     from = c("B", "B", "C", "D", "D", "E"),
                     to = c("C", "D", "D", "E", "F", "F"),
                     Link.Value = c(1L, 1L, 1L, 1L, 1L,1L)
                   ), .Names = c("from", "to", "Link.Value"),
                   class = "data.frame", row.names = c(NA,6L)))
  g_4_truss <- truss(g, k = 4)
  expect_equal(length(igraph::E(g_4_truss)), 0)
  expect_equal(length(igraph::V(g_4_truss)), 0)
})

test_that("test truss equality for Truss2 dataset", {
  nodes <- read.csv("Truss2_nodes.csv")
  edges <- read.csv("Truss2_edges.csv")
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  g_3_truss <- truss(g, k = 3)
  expect_identical(igraph::as_data_frame(g_3_truss),
                   structure(list(from = c("d", "d", "d", "b", "b", "b", "a",
                                           "a", "a", "a", "c", "c", "g", "g",
                                           "e", "e", "f", "f", "f", "f", "h",
                                           "h", "i"),
                                  to = c("e", "g", "l", "c", "d", "e", "b", "c",
                                         "d", "e", "d", "e", "h", "l", "f", "g",
                                         "g", "h", "i", "j", "i", "j", "j"),
                                  Link.Value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                 1, 1, 1)),
                             .Names = c("from", "to", "Link.Value"),
                             class = "data.frame", row.names = c(NA, 23L)))
  g_4_truss <- truss(g, k = 4)
  expect_identical(igraph::as_data_frame(g_4_truss),
                   structure(list(from = c("d", "b", "b", "b", "a", "a", "a",
                                           "a", "c", "c", "f", "f", "f", "h",
                                           "h", "i"),
                                  to = c("e", "c", "d", "e", "b", "c", "d", "e",
                                         "d", "e", "h", "i", "j", "i", "j", "j"),
                                  Link.Value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                                 1, 1, 1, 1, 1, 1)),
                             .Names = c("from", "to", "Link.Value"),
                             class = "data.frame", row.names = c(NA, 16L)))
  g_5_truss <- truss(g, k = 5)
  expect_identical(igraph::as_data_frame(g_5_truss),
                   structure(list(from = c("d", "b", "b", "b", "a", "a", "a",
                                           "a", "c", "c"),
                                  to = c("e", "c", "d", "e", "b", "c", "d",
                                         "e", "d", "e"),
                                  Link.Value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                             .Names = c("from", "to", "Link.Value"),
                             class = "data.frame", row.names = c(NA, 10L)))
  g_6_truss <- truss(g, k = 6)
  expect_equal(length(igraph::E(g_6_truss)), 0)
  expect_equal(length(igraph::V(g_6_truss)), 0)
})

test_that("test truss equality for political books dataset", {
  nodes <- read.csv("political_nodes.csv")
  edges <- read.csv("political_edges.csv")
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  g <- igraph::simplify(g)
  # Set up attributes for later plotting.
  igraph::E(g)$eid <- seq(igraph::ecount(g))
  igraph::E(g)$weight <- 1
  igraph::E(g)$color <- "gray"
  # Truss.
  g_5_truss <- truss(g, k = 5, color_graph = TRUE)
  plot_graph <- TRUE
  if (plot_graph == TRUE){
    # Plot original graph.
    fixed_layout <- igraph::layout_with_lgl(g)
    plot(g, layout = fixed_layout, vertex.label = NA)  # original

    # Plot graph with truss clustering.
    igraph::E(g)$weight <- 1
    igraph::E(g)$color <- "gray"
    igraph::E(g)[igraph::E(g_5_truss)$eid]$color <- igraph::E(g_5_truss)$color
    igraph::E(g)[igraph::E(g_5_truss)$eid]$weight <- 3
    plot(g, layout = fixed_layout, edge.width = igraph::E(g)$weight, vertex.label = NA,
         edge.color = igraph::E(g)$color, vertex.size = 3,
         main = "Political dataset with 5-truss")  # with truss
  }
})


test_that("test 5-truss equality for Node-100-d083", {
  df <- read.csv("Node100_d083.csv")
  g <- igraph::graph_from_data_frame(d = df, directed = FALSE)
  g <- igraph::simplify(g)
  g_5_truss <- truss(g, k = 5)
  expect_identical(igraph::as_data_frame(g_5_truss),
                   structure(list(from = c("N_62", "N_62", "N_62", "N_62", "N_62",
                                           "N_16", "N_16", "N_16", "N_16", "N_64",
                                           "N_64", "N_14", "N_14", "N_9"),
                                  to = c("N_16", "N_64", "N_14", "N_9", "N_97",
                                         "N_64", "N_14", "N_9", "N_97", "N_14",
                                         "N_97", "N_9", "N_97", "N_97")),
                             .Names = c("from", "to"), class = "data.frame",
                             row.names = c(NA, 14L))
  )
})
