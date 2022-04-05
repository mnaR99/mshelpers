
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
#' @returns Vector of random numbers, with the acceptance rate and optimal \eqn{c} as attributes.
#'
#' @param n number of random numbers
#' @param f interest density function
#' @param g sampling density function
#' @param rg random number generator function with distribution g
#' @param c constant or a vector containing the end-points of the interval to be searched
#' for the maximum constant such that \eqn{f(y)/g(y)<=c} for all \eqn{y}
#' @export

ar_method <- function(n, f, g, rg, c) {

    if (length(c) == 2) {
        c <- optimize(function(x) f(x)/g(x), c, maximum = T)$objective
    }

    r <- numeric()

    iters <- 0

    while (length(r) < n) {
        iters <- iters + 1

        y <- rg(1)
        u <- runif(1)

        if (u <= f(y)/(c*g(y))) {
            r <- append(r, y)
        }
    }

    attr(r, "acc_rate") <- n / iters
    attr(r, "optim_c") <- c

    return(r)
}

#' Composition method
#'
#' \deqn{F(x) = \sum (p_i * F_i(x)) , 1 \le i \le k}
#'
#' @param n number of random numbers
#' @param qfns list of the \eqn{k} quantile functions in order
#' @param p vector of \eqn{k} probabilities in order
#' @export

comp_method <- function(n, qfns, p){

    stopifnot(sum(p) == 1)

    r <- numeric(n)
    cp <- cumsum(p)

    for (i in seq_len(n)) {

        u <- runif(2)
        j <- which(u[1] < cp) %>% min()
        r[i] <- qfns[[j]](u[2])

    }

    return(r)
}

#' Composition method (Continuous)
#'
#' \deqn{F(x) = \int_{-\infty}^{\infty} F_{X|Y=y}(x,y) f_{Y}(y) dy}
#'
#' @param n number of random numbers
#' @param qfy quantile function of Y
#' @param qfx quantile function of X|Y, where the first argument is the probability and the second
#' is y
#' @export

comp_method_c <- function(n, qfy, qfx) {

    r <- numeric(n)

    for (i in seq_len(n)) {

        u <- runif(2)
        r[i] <- qfx(u[2], qfy(u[1]))

    }

    return(r)
}

#' Composition method (Discrete)
#'
#' \deqn{Pr(X = x) = a * p_{x} + (1-a) * q_{x}}
#'
#' @param n number of random numbers
#' @param a double constant
#' @param qpx quantile function of px
#' @param qqx quantile function of qx
#' @export

comp_method_d <- function(n, a, qpx, qqx) {

    r <- numeric(n)

    for (i in seq_len(n)) {

        u <- runif(2)

        if (u[1] < a) {
            r[i] <- qpx(u[2])
        } else {
            r[i] <- qqx(u[2])
        }
    }

    return(r)
}

#' Convolution method
#'
#' \deqn{Y ~ \sum (\beta_i * X_i) , 1 \le i \le k}
#'
#' @param n number of random numbers
#' @param y function of the definition of y in terms of \eqn{x_i}
#' @param qfns list of the \eqn{k} quantile functions in order
#' @export

convol_method <- function(n, y, qfns) {

    k <- length(qfns)

    r <- numeric(n)

    for (i in seq_len(n)) {
        r[i] <- purrr::map2_dbl(qfns, runif(k), function(qfn, u) qfn(u)) %>% y()
    }

    return(r)
}

#' Box–Muller transform
#'
#' Random number generator for pairs of independent, standard, normal distributions.
#'
#' @param n number of random numbers
#' @export

rnorm_boxmuller <- function(n) {

    u1 <- runif(n)
    u2 <- runif(n)
    z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)

    return(cbind(z1, z2))
}

#' Multivariate Normal Distribution
#'
#' Random number generator for the multivariate normal distribution with mean equal to
#' \eqn{u} and covariance matrix \eqn{S}.
#'
#' @param n number of random numbers
#' @param u mean vector
#' @param S covariance matrix
#' @export

rmvnorm <- function(n, u, S) {

    d <- length(u)
    Z <- matrix(rnorm(n * d), n, d)
    C <- chol(S)
    X <- t(Z %*% C) + u

    return(t(X))
}
