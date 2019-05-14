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
#' @inheritParams naive_opening
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
