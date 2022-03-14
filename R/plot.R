
#' Histogram
#'
#' Plots a histogram with the possibility of overlaying a function
#'
#' @param v numeric vector
#' @param fun function to plot
#' @param args list of arguments passed to fun
#' @param ... other arguments passed on to \code{\link[ggplot2]{geom_histogram}}
#' @export

plot_continuous <- function(v, fun = NULL, args = NULL, fill = "#C3395B", ...){

    if (!is.null(fun)) {
        plot_fun <-
            ggplot2::stat_function(
                geom = "line",
                fun = fun,
                args = args,
                n = 5e3
            )
    } else {
        plot_fun <- NULL
    }

    ggplot2::ggplot() +
        ggplot2::geom_histogram(
            ggplot2::aes({{v}}, ..density..),
            fill = fill,
            ...
        ) +
        plot_fun
}


#' Barplot
#'
#' Plots a barplot with the possibility of overlaying a discrete function
#'
#' @param v numeric vector
#' @param fun function to graph
#' @param args list of arguments passed to fun
#' @param n desired number of breaks in x axis. You may get slightly more or fewer breaks
#' that requested.
#' @param ... other arguments passed on to \code{\link[ggplot2]{stat_count}}
#' @export

plot_discrete <- function(v, fun = NULL, args = NULL, fill = "#C3395B", n = 5, ...) {

    if (!is.null(fun)) {
        plot_fun <-
            ggplot2::stat_function(
                geom = "point",
                fun = fun,
                args = args,
                xlim = range(v),
                n = diff(range(v)) + 1
            )
    } else {
        plot_fun <- NULL
    }

    ggplot2::ggplot() +
        ggplot2::stat_count(
            ggplot2::aes({{v}}, ..prop..),
            fill = fill,
            ...
        ) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = n)) +
        plot_fun
}
