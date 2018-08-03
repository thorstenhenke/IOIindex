is_sociomatrix <- function(x) {
    is.matrix(x) && isSymmetric(x) && all((x == 0) | (x == 1)) && (all(diag(x) == 0))
}

is_ordinal <- function(v) {
    # Ordinal OR Nominal
    is.factor(v) || is_integer(v)
}

is_integer <- function(v) {
    is.integer(v) || all(v %% 1 == 0)
}