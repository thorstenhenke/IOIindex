Eijk_Rintern <- function(x, v, m) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))

    m <- ifelse(isSymmetric(x), "out", match.arg(m, several.ok = FALSE))

    if (m == "in") {
        wijk <- 1 + colSums(x * outer(v, v, "=="))
    } else {
        wijk <- 1 + rowSums(x * outer(v, v, "=="))
    }

    log(wijk/mean(wijk))
}

Aijk_Rintern <- function(x, v, m) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))

    m <- ifelse(isSymmetric(x), "out", match.arg(m, several.ok = FALSE))

    if (m == "in") {
        wijk <- 1 + colSums(x * outer(v, v, "!="))
    } else {
        wijk <- 1 + rowSums(x * outer(v, v, "!="))
    }

    log(wijk/mean(wijk))
}

Ii_Rintern <- function(x, v, m) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    Aijk_Rintern(x, v, m) - Eijk_Rintern(x, v, m)
}


EReitz_intern <- function(x, v, logscale = FALSE) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    xg <- outer(v, v, "==")
    re <- rowSums(x * xg)
    rn <- rowSums(xg)
    if (logscale) {
        e <- log(re + 1) - log(rn)
    } else {
        e <- re/(rn - 1)
    }
    e
}

AReitz_intern <- function(x, v, logscale = FALSE) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    xg <- outer(v, v, "!=")
    re <- rowSums(x * xg)
    rn <- rowSums(xg)
    if (logscale) {
        a <- log(re + 1) - log(rn + 1)
    } else {
        a <- re/rn
    }
    a
}


balance_intern <- function(x, v) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    vg <- outer(v,v,"==")

    e <- rowSums(x * vg)
    a <- rowSums(x * !vg)

    log(a + 1) - log(e + 1)
}
