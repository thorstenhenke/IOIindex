is_sociomatrix <- function(x) {
    #is.matrix(x) && isSymmetric(x) && all((x == 0) | (x == 1)) && (all(diag(x) == 0))
    is.matrix(x) && all((x == 0) | (x == 1)) && (all(diag(x) == 0))
}

is_nominal <- function(v) {
    is.factor(v) || is_integer(v)
}

is_integer <- function(v) {
    is.integer(v) || all(v %% 1 == 0)
}

apply_list_matrix <- function(fun, net, v, mode, depth = 0) {
    stopifnot(is.list(net) && is.list(v))
    stopifnot(length(net) == length(v))
    stopifnot(all(sapply(net, nrow) == sapply(v, length)))

    depth <- depth + 1

    n <- length(net)
    al <- vector(mode = "list", length = n)

    for (i in 1:n) {
        al[[i]] <- fun(net[[i]], v[[i]], mode, depth = depth)
    }

    compose(al)
}

apply_list <- function(fun, net, vname, mode, depth = 0) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    depth <- depth + 1

    n <- length(net)
    al <- vector(mode = "list", length = n)
    for (i in 1:n) {
        al[[i]] <- fun(net[[i]], vname, mode, depth = depth)
    }

    compose(al)
}

compose <- function(xl) {
    lbl <- names(xl)
    if (is.null(lbl)) {
        lbl <- 1:length(xl)
    }

    ltyp <- sapply(xl, class)

    if (all(ltyp == "data.frame")) {
        lbl <- rep(lbl, times = sapply(xl, nrow))
        val <- do.call("rbind", xl)
        res <- cbind.data.frame(id = lbl, val, row.names = NULL)

    } else if (all((ltyp == "numeric") | (ltyp == "integer") | (ltyp == "character"))) {
        lbl <- rep(lbl, times = sapply(xl, length))
        res <- data.frame(id = lbl, val = unlist(xl), row.names = NULL)
    }

    res
}

print_err_msg <- function(fun_name, x) {
    cc <- paste(class(x), collapse = " ")
    stop(paste0("Method ", fun_name, " is not applicable to data of class ", cc))
}


extractor_network <- function(net, vname) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop(paste0("Attributename <", vname, "> is either not of type character or a vector of characters."))
    }

    if (!(vname %in% network::list.vertex.attributes(x = net))) {
        stop(paste0("Attributename <", vname, "> was not found in the network's vertex attributes."))
    }

    x <- network::as.sociomatrix(x = net)
    v <- network::get.vertex.attribute(x = net, attrname = vname)

    list(x = x, v = v)
}

extractor_igraph <- function(net, vname) {
  if (!(is.character(vname) && length(vname) == 1)) {
      stop(paste0("Attributename <", vname, "> is either not of type character or a vector of characters."))
  }

  if (!(vname %in% igraph::vertex_attr_names(graph = net))) {
      stop(paste0("Attributename <", vname, "> was not found in the network's vertex attributes."))
  }

  x <- as.matrix(igraph::as_adjacency_matrix(graph = net))
  v <- igraph::vertex_attr(graph = net, name = vname)

  list(x = x, v = v)
}
