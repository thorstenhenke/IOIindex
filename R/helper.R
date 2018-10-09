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

apply_list <- function(fun, net, vname, depth = 0) {
    if (!(is.character(vname) && length(vname) == 1)) {
        stop("Fehler")
    }

    depth <- depth + 1

    n <- length(net)
    al <- vector(mode = "list", length = n)
    for (i in 1:n) {
        al[[i]] <- fun(net[[i]], vname, depth = depth)
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
