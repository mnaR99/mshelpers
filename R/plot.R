
#' Histogram
#'
#' Plots a histogram with the possibility of overlaying a function
#'
#' @param v numeric vector
#' @param fun function to plot
#' @param args list of arguments passed to fun
#' @export

plot_continuous <- function(v, fun = NULL, args = NULL, ...){

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

    tibble::tibble(x = v) %>%
        ggplot2::ggplot() +
        ggplot2::geom_histogram(
            ggplot2::aes(x, ..density..),
            fill = "#C3395B",
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
#' @export

plot_discrete <- function(v, fun = NULL, args = NULL, ...){

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

    tibble::tibble(x = v) %>%
        ggplot2::ggplot() +
        ggplot2::stat_count(
            ggplot2::aes(x, ..prop..),
            fill = "#C3395B",
            ...
        ) +
        plot_fun
}
