#' Social homophily Index by McCormick et al.
#'
#' `McCormick()` calculates a homophily index based on a given network and node attribute.
#' This index was invented by McCormick, Cappella, Hughes, Gallagher (2015). DOI: 10.1177/0272431614547051. During the developement of the IOIndex this index was used for validation. Thus both indices, the IOIndex and McCormick's index, should usually be moderatedly correlated. Further explanations regardings this index can be found in the
#' corresponding paper.
#'
#' @param net Network of type matrix, igraph, network or a list of objects of these datatypes.
#' The list objects can even be mixed, meaning that some objects are of type network, some of type
#' igraph, etc. The networks can either be directed or undirected. In case of a directed network it
#' is recommended to specify whether the indices should be calculated based on the incoming network
#' ties or outgoing network ties.
#' @param vname character variable indicating which variable should be used for grouping. This parameter
#' can only be used if net is of type igraph, network or list. Please note, that in case you are
#' using the list input, the variable has to be present in each of the list objects. This parameter
#' can only be used if net is of type igraph, network or list.
#' @param v vector of node attributes. This parameter is only relevant if net is of type matrix. Otherwise
#' this parameter should be ignored and left as it is.
#' @param ... Potential additional paramters. .
#' @param depth this paramter should not be manipulated! It controls the recursion depth of the function
#' in case net is of type list.
#' @examples
#' # Please consult the documentation for the social_closing, social_opneing, etc. indices
#' @return a numeric vector of social opening indices for each indvidual in the network.
#' @export
McCormick <- function(net, ..., depth = 0) {
    UseMethod("McCormick")
}

#' @describeIn McCormick net is of type list
#' @export
McCormick.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(McCormick, net, vname, depth)
    } else {
        mi <- apply_list(McCormick, net, vname, depth)
    }
    mi
}

#' @describeIn McCormick net is of type network
#' @export
McCormick.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    McCormick_intern(obj$x, obj$v, ...)
}

#' @describeIn McCormick net is of type igraph
#' @export
McCormick.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    McCormick_intern(obj$x, obj$v, ...)
}

#' @describeIn McCormick net is of type network
#' @export
McCormick.matrix <- function(net, v, ...) {
    McCormick_intern(net, v, ...)
}

#' @describeIn McCormick error case
#' @export
McCormick.default <- function(net, ...) {
    print_err_msg("McCormick()", net)
}