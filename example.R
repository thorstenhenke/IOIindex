library(network)
library(igraph)

data("faux.magnolia.high", package = "ergm")
fmh <- faux.magnolia.high ; rm(faux.magnolia.high)

x <- as.sociomatrix(fmh)
v <- get.vertex.attribute(fmh, "Grade")

gg <- graph_from_adjacency_matrix(x)
vertex_attr(gg, "Grade") <- v

social_closing(fmh, "Grade")
social_closing(gg, "Grade")
social_closing(list(fmh, fmh), "Grade")
social_closing(list(gg, fmh), "Grade")

social_closing(x, v)
