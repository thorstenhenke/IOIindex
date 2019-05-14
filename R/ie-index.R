#' TITLE
#'
#' `IE_index()` calculates a homophily index based on a given network and node attribute.
#' This index was invented by McCormick, Cappella, Hughes, Gallagher (2015). DOI: 10.1177/0272431614547051. During the developement of the IOIndex this index was used for validation.
#' Thus both indices, the IOIndex and McCormick's index, should usually be moderatedly correlated.
#'
#' @inheritParams social_closing
#' @return a numeric vector of ???
IE_index <- function(net, ..., depth = 0) {
    UseMethod("IE_index")
}

#' @describeIn IE_index net is of type list
IE_index.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(ie_index_intern, net, vname, depth)
    } else {
        mi <- apply_list(ie_index_intern, net, vname, depth)
    }
    mi
}

#' @describeIn IE_index net is of type network
IE_index.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    ie_index_intern(obj$x, obj$v, ...)
}

#' @describeIn IE_index net is of type igraph
IE_index.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    ie_index_intern(obj$x, obj$v, ...)
}

#' @describeIn IE_index net is of type network
IE_index.matrix <- function(net, v, ...) {
    ie_index_intern(net, v, ...)
}

#' @describeIn IE_index error case
IE_index.default <- function(x, ...) {
    print_err_msg("IE_index()", x)
}