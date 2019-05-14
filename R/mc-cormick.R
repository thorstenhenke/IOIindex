#' Social homophily Index
#'
#' `McCormick()` calculates a homophily index based on a given network and node attribute.
#' This index was invented by McCormick, Cappella, Hughes, Gallagher (2015). DOI: 10.1177/0272431614547051. During the developement of the IOIndex this index was used for validation.
#' Thus both indices, the IOIndex and McCormick's index, should usually be moderatedly correlated.
#'
#' @inheritParams social_closing
#' @return a numeric vector of social opening indices for each indvidual in the network.
McCormick <- function(net, ..., depth = 0) {
    UseMethod("McCormick")
}

#' @describeIn McCormick net is of type list
McCormick.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(McCormick, net, vname, depth)
    } else {
        mi <- apply_list(McCormick, net, vname, depth)
    }
    mi
}

#' @describeIn McCormick net is of type network
McCormick.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    McCormick_intern(obj$x, obj$v, ...)
}

#' @describeIn McCormick net is of type igraph
McCormick.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    McCormick_intern(obj$x, obj$v, ...)
}

#' @describeIn McCormick net is of type network
McCormick.matrix <- function(net, v, ...) {
    McCormick_intern(net, v, ...)
}

#' @describeIn McCormick error case
McCormick.default <- function(x, ...) {
    print_err_msg("McCormick()", x)
}