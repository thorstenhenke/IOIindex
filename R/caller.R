social_all <- function(net, vname) {
    a <- social_closing(net, vname)
    e <- social_opening(net, vname)

    # Check for the case that net is a list!!

    data.frame(closing = a, opening = e, integration = a - e)
}


social_closing <- function(net, ..., depth = 0) {
    stopifnot(depth <= 1)
    UseMethod("social_closing")
}

social_closing.list <- function(net, vname, depth = 0) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    depth <- depth + 1

    n <- length(net)
    al <- vector(mode = "list", length = n)
    for (i in 1:n) {
        al[[i]] <- social_closing(net[[i]], vname, depth = depth)
    }

    compose(al)
}

social_closing.network <- function(net, vname, ...) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% network::list.vertex.attributes(x = net))) {
        stop("Fehler")
    }

    x <- network::as.sociomatrix(x = net)
    v <- network::get.vertex.attribute(x = net, attrname = vname)

    Aijk_Rintern(x, v)
}

social_closing.igraph <- function(net, vname, ...) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% igraph::vertex_attr_names(graph = net))) {
        stop("Fehler")
    }

    x <- as.matrix(igraph::as_adjacency_matrix(graph = net))
    v <- igraph::vertex_attr(graph = net, name = vname)

    Aijk_Rintern(x, v)
}

social_closing.matrix <- function(net, v, ...) {
    Aijk_Rintern(net, v)
}

social_closing.default <- function(x, depth = 0, ...) {
    cc <- paste(class(x), collapse = " ")
    stop(paste0("Method social_closing() is not applicable to data of class ", cc))
}


social_opening <- function(net, ...) {
    UseMethod("social_opening")
}

social_opening.network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% network::list.vertex.attributes(x = net))) {
        stop("Fehler")
    }

    x <- network::as.sociomatrix(x = net)
    v <- network::get.vertex.attribute(x = net, attrname = vname)

    Eijk_Rintern(x, v)
}

social_opening.igraph <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% igraph::vertex_attr_names(graph = net))) {
        stop("Fehler")
    }

    x <- as.matrix(igraph::as_adjacency_matrix(graph = net))
    v <- igraph::vertex_attr(graph = net, name = vname)

    Eijk_Rintern(x, v)
}

social_opening.matrix <- function(net, v) {
    Eijk_Rintern(net, v)
}


social_integration <- function(net, ...) {
    UseMethod("social_integration")
}

social_integration.network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% network::list.vertex.attributes(net))) {
        stop("Fehler")
    }

    x <- network::as.sociomatrix(net)
    v <- network::get.vertex.attribute(net, vname)

    Ii_Rintern(x, v)
}

social_integration.igraph <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% igraph::vertex_attr_names(graph = net))) {
        stop("Fehler")
    }

    x <- as.matrix(igraph::as_adjacency_matrix(graph = net))
    v <- igraph::vertex_attr(graph = net, name = vname)

    Ii_Rintern(x, v)
}

social_integration.matrix <- function(net, v) {
    Ii_Rintern(net, v)
}
