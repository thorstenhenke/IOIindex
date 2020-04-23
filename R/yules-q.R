#' Yule's Q
#'
#' `Yules_q()` calculates a homophily index based on a given network and node attribute.
#'
#' Yules_q can be used as an alternative for the well known EI index by Krackhardt and Stern, which
#' is also implemeted in this package. In contrast to the EI Index, Yule's Q also accounts for
#' the distribution of non-ties. For further information on this index and its comparison to the EI
#' index see Borgatti, S. P., Everett, M. G., & Johnson, Jeffrey C. (2013). Analyzing social networks.
#' Los Angeles, et al.: SAGE Publications. The EI index by Krackhardt & Stern (1988)
#' DOI: 10.2307/2786835. was used during the developement of the IOIindex as a validation measure. Yule's Q
#' was not during the developement of the package but in principle it could also have served as a
#' validation for our new index.
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
#' @param depth This paramter should not be manipulated! It controls the recursion depth of the function
#' in case net is of type list.
#' @examples
#' # Please consult the documentation for the social_closing, social_opneing, etc. indices
#'
#' @return A numeric vector of a normalized difference for each individual between
#' the number of in and outgroup ties and non-ties.
#' @export
Yules_q <- function(net, ..., depth = 0) {
    UseMethod("Yules_q")
}

#' @describeIn Yules_q net is of type list
#' @export
Yules_q.list <- function(net, vname, ..., depth = 0) {
    if (is.matrix(net[[1]])) {
        mi <- apply_list_matrix(yules_q_intern, net, vname, depth)
    } else {
        mi <- apply_list(yules_q_intern, net, vname, depth)
    }
    mi
}

#' @describeIn Yules_q net is of type network
#' @export
Yules_q.network <- function(net, vname, ...) {
    obj <- extractor_network(net, vname)
    yules_q_intern(obj$x, obj$v, ...)
}

#' @describeIn Yules_q net is of type igraph
#' @export
Yules_q.igraph <- function(net, vname, ...) {
    obj <- extractor_igraph(net, vname)
    yules_q_intern(obj$x, obj$v, ...)
}

#' @describeIn Yules_q net is of type network
#' @export
Yules_q.matrix <- function(net, v, ...) {
    yules_q_intern(net, v, ...)
}

#' @describeIn Yules_q error case
#' @export
Yules_q.default <- function(net, ...) {
    print_err_msg("Yules_q()", net)
}
