#' Naive in- and outgroup index
#'
#' This index was described and used in various papers (see e.g. Reitz, Asendorpf & Motti-Stefanidi, 2015, DOI: 10.1177/0165025414567008) For this index a ratio
#' between the actual number relationships to members of the in- or outgroup is compared to
#' the maximum possible number of connections within the in- or outgroup
#' (aka the size of the in- or outgroup).
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
#' @return a numeric vector of real valued ratios for each indvidual in the network.
#' @export
#' @examples
#' # Please consult the documentation for the social_closing, social_opneing, etc. indices
#'
#' @export
naive_opening <- function(net, ...) {
    UseMethod("naive_opening")
}

#' @describeIn naive_opening net is of type list
#' @export
naive_opening.list <- function(net, vname, ..., depth = 0) {
    apply_list(naive_opening, net, vname, depth)
}
#' @describeIn naive_opening net is of type network
#' @export
naive_opening.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    AReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type igraph
#' @export
naive_opening.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    AReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type matrix
#' @export
naive_opening.matrix <- function(net, v, ...) {
    AReitz_intern(net, v)
}
#' @describeIn naive_opening error case
#' @export
naive_opening.default <- function(net, depth = 0, ...) {
    print_err_msg("naive_opening()", net)
}

#' Naive in- and outgroup index
#'
#' This index was described and used in various papers (see e.g. Reitz, Asendorpf & Motti-Stefanidi, 2015, DOI: 10.1177/0165025414567008) For this index a ratio
#' between the actual number relationships to members of the in- or outgroup is compared to
#' the maximum possible number of connections within the in- or outgroup
#' (aka the size of the in- or outgroup).
#'
#' @inheritParams naive_opening
#' @return a numeric vector of real valued ratios for each indvidual in the network.
#' @examples
#' # Please consult the documentation for the social_closing, social_opneing, etc. indices
#'
#' @export
naive_closing <- function(net, ...) {
    UseMethod("naive_closing")
}

#' @describeIn naive_opening net is of type list
#' @export
naive_closing.list <- function(net, vname, ..., depth = 0) {
    apply_list(naive_closing, net, vname, depth)
}

#' @describeIn naive_opening net is of type network
#' @export
naive_closing.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    EReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type igraph
#' @export
naive_closing.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    EReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type matrix
#' @export
naive_closing.matrix <- function(net, v, ...) {
    EReitz_intern(net, v)
}
#' @describeIn naive_opening error case
#' @export
naive_closing.default <- function(net, ...) {
    print_err_msg("naive_closing()", net)
}
