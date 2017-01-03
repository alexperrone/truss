# Truss

## Overview

The `truss` package allows one to compute the k-truss clustering algorithm. Currently, this is
only supported on [igraph](https://github.com/igraph/rigraph) objects.

The output of the `truss` function is a subgraph such that every edge is in
at least (k-2) triangles in the subgraph.

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
