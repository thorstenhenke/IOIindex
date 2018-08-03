Eijk_Rintern <- function(x, v) {
    stopifnot(is_sociomatrix(x) && is_ordinal(v))
    wijk <- 1 + rowSums(x * outer(v, v, "=="))
    log(wijk/mean(wijk))
}

Aijk_Rintern <- function(x, v) {
    stopifnot(is_sociomatrix(x) && is_ordinal(v))
    wijk <- 1 + rowSums(x * outer(v, v, "!="))
    log(wijk/mean(wijk))
}

Ii_Rintern <- function(x, v) {
    Aijk_Rintern(x, v) - Eijk_Rintern(x, v)
}
