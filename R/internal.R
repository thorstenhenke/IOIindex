Eijk_Rintern <- function(x, v) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    wijk <- 1 + rowSums(x * outer(v, v, "=="))
    log(wijk/mean(wijk))
}

Aijk_Rintern <- function(x, v) {
    stopifnot(is_sociomatrix(x) && is_nominal(v))
    wijk <- 1 + rowSums(x * outer(v, v, "!="))
    log(wijk/mean(wijk))
}

Ii_Rintern <- function(x, v) {
    Aijk_Rintern(x, v) - Eijk_Rintern(x, v)
}


EReitz_intern <- function(x, v, logscale = FALSE) {
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

AReitz_intern <- function(x, v) {
    xg <- outer(v, v, "=!")
    re <- rowSums(x * xg)
    rn <- rowSums(xg)
    if (logscale) {
        a <- log(re + 1) - log(rn + 1)
    } else {
        a <- re/rn
    }
    a
}

# Idea
wa <- rowSums(m * outer(v,v,"=="))
we <- rowSums(m * outer(v,v,"!="))
log(wa + 1) - log(we + 1)
