library(statnet)

data("faux.magnolia.high")
fmh <- faux.magnolia.high ; rm(faux.magnolia.high)

x <- as.sociomatrix(fmh)
v <- get.vertex.attribute(fmh, "Grade")

soc_closing(fmh, "Grade")
soc_closing(x, v)

# ??? Igraph??? => List?