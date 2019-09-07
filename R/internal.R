assert_data_validity <- function(x, v) {
    stopifnot(no_missings(x))
    stopifnot(is_sociomatrix(x))
    stopifnot(is_nominal(v))
}

# Documented in: <fill in citation after publication>
Eijk_Rintern <- function(x, v, m) {
    assert_data_validity(x, v)
    m <- if (isSymmetric(x)) "out"

    if (m == "in") {
        wijk <- 1 + colSums(x * outer(v, v, "=="))
    } else {
        wijk <- 1 + rowSums(x * outer(v, v, "=="))
    }

    log(wijk) - log(mean(wijk))
}

Aijk_Rintern <- function(x, v, m) {
    assert_data_validity(x, v)

    m <- if (isSymmetric(x)) "out"

    if (m == "in") {
        wijk <- 1 + colSums(x * outer(v, v, "!="))
    } else {
        wijk <- 1 + rowSums(x * outer(v, v, "!="))
    }

    log(wijk) - log(mean(wijk))
}

Ii_Rintern <- function(x, v, m) {
    assert_data_validity(x, v)
    Aijk_Rintern(x, v, m) - Eijk_Rintern(x, v, m)
}

# Documented in: Reitz, Asendorpf & Motti-Stefanidi, 2015, DOI: 10.1177/0165025414567008
EReitz_intern <- function(x, v) {
    assert_data_validity(x, v)
    xg <- outer(v, v, "==")

    m <- if (isSymmetric(x)) "out"

    if (m == "in") {
        re <- colSums(x * xg)
        rn <- colSums(xg) - 1  # since xg also tests the diagonal for equality, the sum is of by one
    } else {  # m == "out
        re <- rowSums(x * xg)
        rn <- rowSums(xg) - 1  # see comment above
    }

    re/rn
}

AReitz_intern <- function(x, v, m) {
    assert_data_validity(x, v)
    xg <- outer(v, v, "!=")

    m <- if (isSymmetric(x)) "out"

    if (m == "in") {
        re <- colSums(x * xg)
        rn <- colSums(xg)
    } else {  # m == "out
        re <- rowSums(x * xg)
        rn <- rowSums(xg)
    }

    re/rn
}

# Documented in: McCormick, Cappella, Hughes, Gallagher (2015). DOI: 10.1177/0272431614547051.
McCormick_intern <- function(x, v, logscale = FALSE) {
    assert_data_validity(x, v)
    df  <- length(v) - 1
    nf  <- rowSums(x)

    vv  <- outer(v, v, "==")
    srp <- rowSums(vv)
    srf <- rowSums(x * vv)

    if (!logscale) {
        return((srf * df) / (srp * nf))
    }

    (log(srf) + log(df)) - (log(srp) - log(nf))
}

# Documented in: Borgatti, S. P., Everett, M. G., & Johnson, Jeffrey C. (2013). Analyzing social networks.
#' Los Angeles, et al.: SAGE Publications.
ie_index_intern <- function(x, v, m = c("in", "out")) {
    assert_data_validity(x, v)

    m <- match.arg(m, several.ok = FALSE)

    vv <- outer(v, v, "==")

    if (isSymmetric(x) || (m == "out")) {
        sum_func <- colSums
    } else {
        sum_func <- rowSums
    }

    a <- sum_func(x *  vv)
    b <- sum_func(x * !vv)

    (b - a) / (b + a)
}

# Documented in: Borgatti, S. P., Everett, M. G., & Johnson, Jeffrey C. (2013). Analyzing social networks.
#' Los Angeles, et al.: SAGE Publications.
yules_q_intern <- function(x, v, m = c("in", "out")) {
    assert_data_validity(x, v)

    m <- match.arg(m, several.ok = FALSE)

    vv <- outer(v, v, "==")

    if (isSymmetric(x) || (m == "out")) {
        sum_func <- colSums
    } else {
        sum_func <- rowSums
    }

    a <- sum_func( x *  vv)
    b <- sum_func( x * !vv)
    c <- sum_func(!x *  vv)
    d <- sum_func(!x * !vv)

    (a*d - b*c) / (a*d + b*c)
}