# IOIindex

The IOIndex is a collection of homophily measures for social networks. It quantifies and  expresses the tendency of members in a social network to connect to other members outside or  inside a certain group. These groups may be defined by social categories like gender,  ethnicity, language, etc. In contrast to similar homophily indices the IOIndex does not compare  the in- and out-group orientation of a specific person with the number of possible in- and out-group  connections, but it compares it to the average in- and out-group orientation of the other  network members. The IOIndex thus can be understood as a measure of activiy for a specific network  member. In addition to the newly devoloped index we also inclued implementations of several older indices  as e.g. the well-known IE index and Yule's Q. These additional indices  can be used for validating  and double checking the results of the IOIndex procedures. 

## Installation

You can install the developement version of IOIndex from [GitHub](https://github.com/thorstenhenke/IOIndex) with:

``` r
devtools::install_github("https://github.com/thorstenhenke/IOIndex")
```

## Example

This is a basic example which shows you how to use IOIndex. For more information please consult the acompanying documentation. 

``` r
library(IOIndex)
data("faux.magnolia.high", package = "ergm")
fmh <- faux.magnolia.high ; rm(faux.magnolia.high)
social_closing(fmh, "Grade")

x <- network::as.sociomatrix(fmh)
v <- network::get.vertex.attribute(fmh, "Grade")
social_closing(x, v)

gg <- igraph::graph_from_adjacency_matrix(x)
igraph::vertex_attr(gg, "Grade") <- v

social_closing(list(fmh, fmh), "Grade")
social_closing(list(gg, fmh), "Grade")
```

