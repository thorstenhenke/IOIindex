soc_closing <- function(net, ...) {
    UseMethod("soc_closing")
}

soc_closing.network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% list.vertex.attributes(net))) {
        stop("Fehler")
    }

    x <- as.sociomatrix(net)
    v <- get.vertex.attribute(net, vname)

    Aijk_Rintern(x, v)
}

soc_closing.matrix <- function(net, v) {
    Aijk_Rintern(net, v)
}


soc_opening <- function(net, ...) {
    UseMethod("soc_opening")
}

soc_opening.network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% list.vertex.attributes(net))) {
        stop("Fehler")
    }

    x <- as.sociomatrix(net)
    v <- get.vertex.attribute(net, vname)

    Eijk_Rintern(x, v)
}

soc_opening.matrix <- function(net, v) {
    Eijk_Rintern(net, v)
}


soc_integration <- function(net, ...) {
    UseMethod("soc_integration")
}

soc_integration.network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    if (!(vname %in% list.vertex.attributes(net))) {
        stop("Fehler")
    }

    x <- as.sociomatrix(net)
    v <- get.vertex.attribute(net, vname)

    Ii_Rintern(x, v)
}

soc_integration.matrix <- function(net, v) {
    Ii_Rintern(net, v)
}
