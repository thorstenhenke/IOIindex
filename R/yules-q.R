#' TITLE
#'
#' `McCormick()` calculates a homophily index based on a given network and node attribute.
#' This index was invented by McCormick, Cappella, Hughes, Gallagher (2015). DOI: 10.1177/0272431614547051. During the developement of the IOIndex this index was used for validation.
#' Thus both indices, the IOIndex and McCormick's index, should usually be moderatedly correlated.
#'
#' @inheritParams social_closing
#' @return a numeric vector of ???
Yules_q <- function(net, ..., depth = 0) {
    UseMethod("Yules_q")
}

#' @describeIn Yules_q net is of type list
Yules_q.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(yules_q_intern, net, vname, depth)
    } else {
        mi <- apply_list(yules_q_intern, net, vname, depth)
    }
    mi
}

#' @describeIn Yules_q net is of type network
Yules_q.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    yules_q_intern(obj$x, obj$v, ...)
}

#' @describeIn Yules_q net is of type igraph
Yules_q.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    yules_q_intern(obj$x, obj$v, ...)
}

#' @describeIn Yules_q net is of type network
Yules_q.matrix <- function(net, v, ...) {
    yules_q_intern(net, v, ...)
}

#' @describeIn Yules_q error case
Yules_q.default <- function(x, ...) {
    print_err_msg("Yules_q()", x)
}