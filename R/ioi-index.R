#' Social closing, opening and integration coefficients
#'
#' `social_all()` calculates the social closing, opening and
#' integration coefficients for a given network and set of node attributes.
#' @param net Network of either type igraph, network or a list of objects of any of these datatypes.
#' The list objects can even be mixed, meaing that some objects are of type network, some of type
#' igraph, etc. However, the type matrix is not allowed in this function.
#' @param vname character variable indicating the variable that is used for grouping. This parameter
#' can only be used if net is of type igraph, network or list. Please note, that in case you are
#' using the list input, the variable has to be present in each of the list objects.
#' @param mode This parameter will only be evaluated if the network is directed. In case of an undirected
#' network this parameter will be ignored.
#' @return a data.frame with three columns for the social closing, opening and integration coefficients
#' per person in the given network(s).
#' @export
social_all <- function(net, vname, mode = c("in", "out")) {

    if (is.list(net) && !(network::is.network(net) || igraph::is.igraph(net))) {
        a <- apply_list(social_closing, net, vname, mode, depth = 0)
        e <- apply_list(social_opening, net, vname, mode, depth = 0)
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
#' \dontrun{
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
#' }
#' @export
social_closing <- function(net, ..., depth = 0) {
    stopifnot(depth <= 1)
    UseMethod("social_closing")
}

#' @describeIn social_closing net is of type list
#' @export
social_closing.list <- function(net, vname, mode = c("in", "out"), ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        ci <- apply_list_matrix(social_closing, net, vname, mode, depth)
    } else {
        ci <- apply_list(social_closing, net, vname, mode, depth)
    }
    ci
}

#' @describeIn social_closing net is of type network
#' @export
social_closing.network <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_network(net, vname)
    Eijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_closing net is of type igraph
#' @export
social_closing.igraph <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_igraph(net, vname)
    Eijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_closing net is of type matrix
#' @export
social_closing.matrix <- function(net, v, mode = c("in", "out"), ...) {
    Eijk_Rintern(net, v, mode)
}

#' @describeIn social_closing error case
#' @export
social_closing.default <- function(net, depth = 0, ...) {
    print_err_msg("social_closing()", net)
}

#' Social opening coefficient
#'
#' `social_opening()` calculates the social opening coefficient for a given network and node attribute.
#'
#' @inheritParams social_closing
#' @return a numeric vector of social opening indices for each indvidual in the network.
#' @export
social_opening <- function(net, ...) {
    UseMethod("social_opening")
}

#' @describeIn social_closing net is of type list
#' @export
social_opening.list <- function(net, vname, mode = c("in", "out"), ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        oi <- apply_list_matrix(social_opening, net, vname, mode, depth)
    } else {
        oi <- apply_list(social_opening, net, vname, mode, depth)
    }
    oi
}

#' @describeIn social_opening net is of type network
#' @inheritParams social_closing.network
#' @export
social_opening.network <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_network(net, vname)
    Aijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_opening net is of type igraph
#' @inheritParams social_closing.igraph
#' @export
social_opening.igraph <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_igraph(net, vname)
    Aijk_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_opening net is of type matrix
#' @inheritParams social_closing.matrix
#' @export
social_opening.matrix <- function(net, v, mode = c("in", "out"), ...) {
    Aijk_Rintern(net, v, mode)
}

#' @describeIn social_opening error case
#' @export
social_opening.default <- function(net, depth = 0, ...) {
    print_err_msg("social_opening()", net)
}

#' Social integration coefficient
#'
#' `social_integration()` calculates the social integration coefficient for a given network and node attribute.
#'
#' @inheritParams social_closing
#' @return a numeric vector of social integration indices for each indvidual in the network.
#' @export
social_integration <- function(net, ...) {
    UseMethod("social_integration")
}

#' @describeIn social_closing net is of type list
#' @export
social_integration.list <- function(net, vname, mode = c("in", "out"), ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        oi <- apply_list_matrix(social_integration, net, vname, mode, depth)
    } else {
        oi <- apply_list(social_integration, net, vname, mode, depth)
    }
    oi
}

#' @describeIn social_integration net is of type network
#' @export
#' @inheritParams social_closing.network
social_integration.network <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_network(net, vname)
    Ii_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_integration net is of type igraph
#' @inheritParams social_closing.igraph
#' @export
social_integration.igraph <- function(net, vname, mode = c("in", "out"), ...) {
    obj <- extractor_igraph(net, vname)
    Ii_Rintern(obj$x, obj$v, mode)
}

#' @describeIn social_integration net is of type matrix
#' @inheritParams social_closing.matrix
#' @export
social_integration.matrix <- function(net, v, mode = c("in", "out"), ...) {
    Ii_Rintern(net, v, mode)
}

#' @describeIn social_integration error case
#' @export
social_integration.default <- function(net, depth = 0, ...) {
    print_err_msg("social_integration()", net)
}