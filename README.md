# Truss

## Overview

The `truss` package allows one to compute the k-truss clustering algorithm. Currently, this is
only supported on [igraph](https://github.com/igraph/rigraph) objects.

The `truss` function returns a subgraph such that every edge is in
at least (k-2) triangles in the subgraph. The `truss_edge` function returns
a vector indicating, for each edge, the maximal `k`-truss the edge occurs in. 
Truss membership is implemented but should be considered experimental. 

## Installation

```R
# install.packages("devtools")
devtools::install_github("alexperrone/truss")
```

## Usage

```R
library(truss)

g <- sample_gnm(6, 9)
g_truss <- truss(g, k = 3)
```

The truss can be plotted.

![political-books](https://github.com/alexperrone/truss/blob/master/political.png)
