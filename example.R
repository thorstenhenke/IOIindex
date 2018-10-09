
data("faux.magnolia.high", package = "ergm")
fmh <- faux.magnolia.high ; rm(faux.magnolia.high)

x <- network::as.sociomatrix(fmh)
v <- network::get.vertex.attribute(fmh, "Grade")

gg <- igraph::graph_from_adjacency_matrix(x)
igraph::vertex_attr(gg, "Grade") <- v

social_closing(fmh, "Grade")
social_closing(gg, "Grade")
social_closing(list(fmh, fmh), "Grade")
social_closing(list(gg, fmh), "Grade")

social_closing(x, v)
