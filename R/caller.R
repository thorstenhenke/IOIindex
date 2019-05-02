#' Social closing and opening coefficients
#'
#' `social_all()` calculates the social closing, opening and
#' integration coefficients for a given network and set of node attributes.
#' @param net Network of either type igraph, network or a list of objects of any of these datatypes.
#' The list objects can even be mixed, meaing that some objects are of type network, some of type
#' igraph, etc. However, the type matrix is not allowed in this function.
#' @param vname character variable indicating the variable that is used for grouping. This parameter
#' can only be used if net is of type igraph, network or list. Please note, that in case you are
#' using the list input, the variable has to be present in each of the list objects.
#' @return a data.frame with three columns for the social closing, opening and integration coefficients
#' per person in the given network(s).
social_all <- function(net, vname) {

    if (is.list(net) && !(network::is.network(net) || igraph::is.igraph(net))) {
        a <- apply_list(social_closing, net, vname, depth = 0)
        e <- apply_list(social_opening, net, vname, depth = 0)
    } else {
        a <- social_closing(net, vname)
        e <- social_opening(net, vname)
    }

    data.frame(closing = a, opening = e, integration = a - e)
}

#' Social closing coefficient
#'
#' `social_closing()` calculates the social closing coefficient for a given network and node attribute.
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
#' @param mode This parameter will only be evaluated if the network is directed. In case of an undirected
#' network this parameter will be ignored.
#' @param ... Potential additional paramters. .
#' @param depth this paramter should not be manipulated! It controls the recursion depth of the function
#' in case net is of type list.
#' @return a numeric vector of social closing indices for each indvidual in the network.
#' @examples
#'
#' data("faux.magnolia.high", package = "ergm")
#' fmh <- faux.magnolia.high ; rm(faux.magnolia.high)
#' social_closing(fmh, "Grade")
#'
#' x <- network::as.sociomatrix(fmh)
#' v <- network::get.vertex.attribute(fmh, "Grade")
#' social_closing(x, v)
#'
#' gg <- igraph::graph_from_adjacency_matrix(x)
#' igraph::vertex_attr(gg, "Grade") <- v
#'
#' social_closing(list(fmh, fmh), "Grade")
#' social_closing(list(gg, fmh), "Grade")
social_closing <- function(net, ..., depth = 0) {
    stopifnot(depth <= 1)
    UseMethod("social_closing")
}

#' @describeIn social_closing net is of type list
social_closing.list <- function(net, vname, mode = c("out", "in"), depth = 0) {
    if (is.matrix(net[[1]])) {
        ci <- apply_list_matrix(social_closing, net, vname, mode, depth)
    } else {
        ci <- apply_list(social_closing, net, vname, mode, depth)
    }
    ci
}

#' @describeIn social_closing net is of type network
social_closing.network <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_network(net, vname)
    Aijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_closing net is of type igraph
social_closing.igraph <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_igraph(net, vname)
    Aijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_closing net is of type matrix
social_closing.matrix <- function(net, v, mode = c("out", "in"), ...) {
    Aijk_Rintern(net, v, mode)
}

#' @describeIn social_closing error case
social_closing.default <- function(x, depth = 0, ...) {
    print_err_msg("social_closing()", x)
}

#' Social opening coefficient
#'
#' `social_opening()` calculates the social opening coefficient for a given network and node attribute.
#'
#' @inheritParams social_closing
#' @return a numeric vector of social opening indices for each indvidual in the network.
social_opening <- function(net, ...) {
    UseMethod("social_opening")
}

#' @describeIn social_closing net is of type list
social_opening.list <- function(net, vname, mode = c("out", "in"), depth = 0) {
    if (is.matrix(net[[1]])) {
        oi <- apply_list_matrix(social_opening, net, vname, mode, depth)
    } else {
        oi <- apply_list(social_opening, net, vname, mode, depth)
    }
    oi
}

#' @describeIn social_opening net is of type network
#' @inheritParams social_closing.network
social_opening.network <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_network(net, vname)
    Eijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_opening net is of type igraph
#' @inheritParams social_closing.igraph
social_opening.igraph <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_igraph(net, vname)
    Eijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_opening net is of type matrix
#' @inheritParams social_closing.matrix
social_opening.matrix <- function(net, v, mode = c("out", "in"), ...) {
    Eijk_Rintern(net, v, mode)
}

#' @describeIn social_opening error case
social_opening.default <- function(x, depth = 0, ...) {
    print_err_msg("social_opening()", x)
}

#' Social integration coefficient
#'
#' `social_integration()` calculates the social integration coefficient for a given network and node attribute.
#'
#' @inheritParams social_closing
#' @return a numeric vector of social integration indices for each indvidual in the network.
social_integration <- function(net, ...) {
    UseMethod("social_integration")
}

#' @describeIn social_closing net is of type list
social_integration.list <- function(net, vname, mode = c("out", "in"), depth = 0) {
    if (is.matrix(net[[1]])) {
        oi <- apply_list_matrix(social_integration, net, vname, mode, depth)
    } else {
        oi <- apply_list(social_integration, net, vname, mode, depth)
    }
    oi
}

#' @describeIn social_integration net is of type network
#' @inheritParams social_closing.network
social_integration.network <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_network(net, vname)
    Ii_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_integration net is of type igraph
#' @inheritParams social_closing.igraph
social_integration.igraph <- function(net, vname, mode = c("out", "in"), ...) {
    obj <- extractor_igraph(net, vname)
    Ii_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_integration net is of type matrix
#' @inheritParams social_closing.matrix
social_integration.matrix <- function(net, v, mode = c("out", "in"), ...) {
    Ii_Rintern(net, v, mode)
}

#' @describeIn social_integration error case
social_integration.default <- function(x, depth = 0, ...) {
    print_err_msg("social_integration()", x)
}

#' Naive in- and outgroup index
#'
#' This index was described and used in various papers (see e.g. Reitz, Asendorpf & Motti-Stefanidi, 2015, DOI: 10.1177/0165025414567008) For this index a ratio
#' between the actual number relationships to members of the in- or outgroup is compared to
#' the maximum possible number of connections within the in- or outgroup
#' (aka the size of the in- or outgroup).
#'
#' @inheritParams social_closing
naive_opening <- function(net, ...) {
    UseMethod("naive_opening")
}

#' @describeIn naive_opening net is of type list
naive_opening.list <- function(net, vname, depth = 0) {
    apply_list(naive_opening, net, vname, depth)
}
#' @describeIn naive_opening net is of type network
naive_opening.network <- function(net, vname) {
    obj <- extractor_network(net, vname)
    AReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type igraph
naive_opening.igraph <- function(net, vname) {
    obj <- extractor_igraph(net, vname)
    AReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type matrix
naive_opening.matrix <- function(net, v) {
    AReitz_intern(net, v)
}
#' @describeIn naive_opening error case
naive_opening.default <- function(x, depth = 0, ...) {
    print_err_msg("naive_opening()", x)
}

#' Naive in- and outgroup index
#'
#' This index was described and used in various papers (see e.g. Reitz, Asendorpf & Motti-Stefanidi, 2015, DOI: 10.1177/0165025414567008) For this index a ratio
#' between the actual number relationships to members of the in- or outgroup is compared to
#' the maximum possible number of connections within the in- or outgroup
#' (aka the size of the in- or outgroup).
#'
#' @describeIn naive_opening general implementation for S3 class dispatch
naive_closing <- function(net, ...) {
    UseMethod("naive_closing")
}

#' @describeIn naive_opening net is of type list
naive_closing.list <- function(net, vname, depth = 0) {
    apply_list(naive_closing, net, vname, depth)
}

#' @describeIn naive_opening net is of type network
naive_closing.network <- function(net, vname) {
    obj <- extractor_network(net, vname)
    EReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type igraph
naive_closing.igraph <- function(net, vname) {
    obj <- extractor_igraph(net, vname)
    EReitz_intern(obj$x, obj$v)
}
#' @describeIn naive_opening net is of type matrix
naive_closing.matrix <- function(net, v) {
    EReitz_intern(net, v)
}
#' @describeIn naive_opening error case
naive_closing.default <- function(net, ...) {
    print_err_msg("naive_closing()", net)
}

naive_balance <- function(net, ...) {
    UseMethod("naive_balance")
}

naive_balance.list <- function(net, vname, depth = 0) {
    apply_list(naive_balance, net, vname, depth)
}
naive_balance.network <- function(net, vname) {
    obj <- extractor_network(net, vname)
    balance_intern(obj$x, obj$v)
}
naive_balance.igraph <- function(net, vname) {
    obj <- extractor_igraph(net, vname)
    balance_intern(obj$x, obj$v)
}
naive_balance.matrix <- function(net, v) {
    balance_intern(x, v)
}
naive_balance.default <- function(x, depth = 0, ...) {
    print_err_msg("naive_balance()", x)
}


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