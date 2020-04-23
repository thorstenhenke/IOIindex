#' Krackhardt and Sterns' EI index
#'
#' `EI_index()` calculates an inverse homophily index based on a given network and node attribute.
#'
#'  During the developement of the IOIindex this index was used for validation. The index was developed
#'  by Krackhardt & Stern (1988) DOI:  DOI: 10.2307/2786835. Thus both indices, the IOIindex and the
#'  EI-index, should usually correlate moderately and negatively. However, as the EI-Index
#'  is known to be sensitive to group size, the association strength with the IOIindex
#'  should be expected to be smaller than for the other indices in this package.
#'
#'
#' @param net Network of type matrix, igraph, network or a list of objects of these datatypes.
#' The list objects can even be mixed, meaning that some objects are of type network, some of type
#' igraph, etc. The networks can either be directed or undirected. In case of a directed network it
#' is recommended to specify whether the indices should be calculated based on the incoming network
#' ties or outgoing network ties.
#' @param vname Character variable indicating which variable should be used for grouping. This parameter
#' can only be used if net is of type igraph, network or list. Please note, that in case you are
#' using the list input, the variable has to be present in each of the list objects.
#' @param v Vector of node attributes. This parameter is only relevant if net is of type matrix. Otherwise
#' this parameter should be ignored and left as it is.
#' @param ... Potential additional paramters.
#' @param depth This parameter should not be manipulated! It controls the recursion depth of the function
#' in case net is of type list.
#' @examples
#' # Please consult the documentation for the social_closing, social_opneing, etc. indices
#'
#' @return A numeric vector of a normalized difference for each individual between
#' the number of in and outgroup ties.
#' @export
EI_index <- function(net, ..., depth = 0) {
    UseMethod("EI_index")
}

#' @describeIn EI_index net is of type list
#' @export
EI_index.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(ie_index_intern, net, vname, depth)
    } else {
        mi <- apply_list(ie_index_intern, net, vname, depth)
    }
    mi
}

#' @describeIn EI_index net is of type network
#' @export
EI_index.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    ie_index_intern(obj$x, obj$v, ...)
}

#' @describeIn EI_index net is of type igraph
#' @export
EI_index.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    ie_index_intern(obj$x, obj$v, ...)
}

#' @describeIn EI_index net is of type network
#' @export
EI_index.matrix <- function(net, v, ...) {
    ie_index_intern(net, v, ...)
}

#' @describeIn EI_index error case
#' @export
EI_index.default <- function(net, ...) {
    print_err_msg("EI_index()", net)
}
