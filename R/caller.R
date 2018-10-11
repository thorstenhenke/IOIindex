#' Social closing and opening coefficients
#'
#' `social_all()` calculates the social closing, opening and
#' integration coefficients for a given network and node attribute.
#' @param net Network of type igraph, network or a list of objects of these datatypes. The
#' type matrix is not allowed in this function.
#' @param vname character variable indicating the . This parameter can only be used if net is of
#' type igraph, network or list.
#' @return a three column data.frame with the social closing, opening and integration coefficients
#' per person in the network.
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
#' The network can either be directed or undirected.
#' @param vname character variable indicating the . This parameter can only be used if net is of
#' type igraph, network or list.
#' @param v vector of node attributes.
#' @param ... Addtional paramters
#' @param depth this paramter should not be manipulated! It controls the recursion depth of the function
#' In case net is of type list.
#' @return a numeric vector of social closinig indices for each indvidual in the network.
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
social_closing.list <- function(net, vname, depth = 0) {
    apply_list(social_closing, vname, depth)
}

#' @describeIn social_closing net is of type network
social_closing.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    Aijk_Rintern(obj$x, obj$v)
}

#' @describeIn social_closing net is of type igraph
social_closing.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    Aijk_Rintern(x, v)
}

#' @describeIn social_closing net is of type matrix
social_closing.matrix <- function(net, v, ...) {
    Aijk_Rintern(net, v)
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
social_opening.list <- function(net, vname, depth = 0) {
    apply_list(social_opening, vname, depth)
}

#' @describeIn social_opening net is of type network
#' @inheritParams social_closing.network
social_opening.network <- function(net, vname) {
    obj <- extractor_network(net, vname)
    Eijk_Rintern(obj$x, obj$v)
}

#' @describeIn social_opening net is of type igraph
#' @inheritParams social_closing.igraph
social_opening.igraph <- function(net, vname) {
    obj <- extractor_igraph(net, vname)
    Eijk_Rintern(obj$x, obj$v)
}

#' @describeIn social_opening net is of type matrix
#' @inheritParams social_closing.matrix
social_opening.matrix <- function(net, v) {
    Eijk_Rintern(net, v)
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
social_integration.list <- function(net, vname, depth = 0) {
    apply_list(social_integration, vname, depth)
}

#' @describeIn social_integration net is of type network
#' @inheritParams social_closing.network
social_integration.network <- function(net, vname) {
    obj <- extractor_network(net, vname)
    Ii_Rintern(obj$x, obj$v)
}

#' @describeIn social_integration net is of type igraph
#' @inheritParams social_closing.igraph
social_integration.igraph <- function(net, vname) {
    obj <- extractor_igraph(net, vname)
    Ii_Rintern(obj$x, obj$v)
}

#' @describeIn social_integration net is of type matrix
#' @inheritParams social_closing.matrix
social_integration.matrix <- function(net, v) {
    Ii_Rintern(net, v)
}

#' @describeIn social_integration error case
social_integration.default <- function(x, depth = 0, ...) {
    print_err_msg("social_integration()", x)
}
