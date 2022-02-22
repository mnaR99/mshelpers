
#' Linear Congruential Generator
#'
#' \deqn{x_{n} = (ax_{n-1} + c) modulo m}
#'
#' @param n number of random numbers
#' @param seed Seed
#' @param a positive integer
#' @param c positive integer
#' @param m positive integer
#' @export

lcg <- function(n, seed, a, c, m) {
    purrr::accumulate(1:n, ~ (a * . + c) %% m, .init = seed)[-1] / m
}

#' Wichmann-Hill (WH) Random Number Generator
#'
#' The Wichmann-Hill (WH) algorithm is an extension of linear congruential. WH uses three linear
#' congruential equations, adds the results, then takes the decimal part of the sum. For example,
#' if the three WH equations give 0.68, 0.92, and 0.35, then the final result random number is
#' decimal(0.78 + 0.92 + 0.45) = decimal(2.95) = 0.95.
#'
#' @param n number of random numbers
#' @param seed 3 seeds
#' @param a 3 positive integers
#' @param c 3 positive integers
#' @param m 3 positive integers
#' @export

wh <- function(n, seed, a, c, m) {
    x1 <- lcg(n, seed[1], a[1], c[1], m[1])
    x2 <- lcg(n, seed[2], a[2], c[2], m[2])
    x3 <- lcg(n, seed[3], a[3], c[3], m[3])

    (x1+x2+x3) %% 1
}

#' Acceptance-Rejection method
#'
#' @param n number of random numbers
#' @param f interest density function
#' @param g sampling density function
#' @param rg random number generator function with distribution g
#' @param c constant or a vector containing the end-points of the interval to be searched
#' for the maximum constant such that \eqn{f(y)/g(y)<=c} for all y.
#' @export

arm <- function(n, f, g, rg, c) {

    if (length(c) == 2) {
        c <- optimize(function(x) f(x)/g(x), c, maximum = T)$objective
    }

    r <- numeric()

    while (length(r) < n) {
        y <- rg(1)
        u <- runif(1)

        if (u <= f(y)/(c*g(y))) {
            r <- append(r, y)
        }
    }

    return(r)
}
