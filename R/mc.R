
#' Monte Carlo convergence
#'
#' Line plot of the cumulative mean for a vector of Monte Carlo simulations
#'
#' @param x numeric vector
#' @param reference true value to be estimated through Monte Carlo simulations
#' @param ... other arguments passed on to \code{\link[ggplot2]{geom_path}}
#'
#' @export

plot_mc_convergence <- function(x, reference = NULL, ...) {
    ggplot2::ggplot()  +
        ggplot2::geom_hline(yintercept = reference) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(x = seq_along(x), y = cumsum(x)/seq_along(x)),
            ...
        ) +
        ggplot2::labs(
            x = "Iter",
            y = "Mean"
        )
}
