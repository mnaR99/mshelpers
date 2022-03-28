
#' Model support classifications
#'
#' \code{\link[univariateML]{univariateML_models}} grouped by type of support.
#'
#' `real_support`: Cauchy, Gumbel, Laplace, Logistic, Normal, Student t, Generalized Error,
#' Skew Normal, Skew Student t, Skew Generalized Error
#'
#' `positive_real_support`: Beta prime, Exponential, Gamma, Inverse gamma, Inverse Gaussian,
#' Inverse Weibull, Log-logistic, Log-normal, Lomax, Rayleigh, Weibull
#'
#' `zero_one_support`: Beta, Kumaraswamy, Logit-normal
#'
#' Other models:
#' - `lgamma`: Log-gamma (1, \eqn{\infty})
#' - `pareto`: Pareto [b, \eqn{\infty})
#' - `power`: Power [0,a)
#' - `unif`: Uniform (min, max)
#'
#' @name models
#' @export

real_support <- c("cauchy","gumbel","laplace","logis","norm","std","ged","snorm","sstd","sged")

#' @rdname models
#' @export

positive_real_support <- c("betapr","exp","gamma","invgamma","invgauss","invweibull","llogis","lnorm","lomax","rayleigh","weibull")

#' @rdname models
#' @export

zero_one_support <- c("beta","kumar","logitnorm")

#' Fit multiple models
#'
#' Estimate the parameters of specified models
#'
#' @return Returns a tibble with the fitted models and their corresponding log-likelihood,
#' AIC, BIC and p-value of the Kolmogorov-Smirnov test.
#'
#' @param x numeric vector
#' @param dists a character vector containing the distribution models to fit; see
#' help(mshelpers::models)
#' @param quiet hide errors (TRUE, the default), or display them as they occur?
#'
#' @export

fit_mlm <- function(x, dists, quiet = FALSE){

    fits <- paste0("univariateML::ml", dists) %>%
        purrr::map(~ eval(parse(text = .x))) %>%
        purrr::map(~ try(.x(x), silent = TRUE)) %>%
        purrr::set_names(dists)

    error_inds <- purrr::map_lgl(fits, ~ inherits(.x, "try-error"))

    if (any(error_inds)){

        if (!quiet) {
            purrr::walk2(
                names(fits[error_inds]), fits[error_inds],
                ~ message(.x, " model\n", .y[1])
            )
        }

        fits <- fits[!error_inds]
    }

    model_names <- purrr::map_chr(fits, ~ attr(.x, "model"))
    logLik <- purrr::map_dbl(fits, logLik)
    AIC <- purrr::map_dbl(fits, AIC)
    BIC <- purrr::map_dbl(fits, BIC)
    ks_pval <- purrr::map_dbl(fits, ~ ks.test(x, purrr::partial(univariateML::pml, obj = .x))[["p.value"]])

    dplyr::tibble(
        fit = fits,
        model = ifelse(is.na(model_names), dists, model_names),
        logLik,
        AIC,
        BIC,
        minIC = pmin(AIC, BIC),
        ks_pval
    ) %>%
        dplyr::arrange(minIC)

}

#' Plot models
#'
#' Returns a list of plots, each with the data and the fitted model overlaid.
#'
#' The returned list allows, in conjunction with \code{\link[patchwork]{wrap_plots}} or another
#' function from any library of your choice, to combine the resulting plots into one, or simply
#' edit/show each plot separately.
#'
#' @param v numeric vector
#' @param object resulting object from `fit_mlm`
#' @param ... other arguments passed on to \code{\link[ggplot2]{geom_histogram}} for `plot_dml`,
#' to \code{\link[ggplot2]{stat_ecdf}} for `plot_pml`, to \code{\link[ggplot2]{stat_qq}} for
#' `plot_qqml`
#' @param xlim optionally, restrict the range of the function to this range
#'
#' @name plot_models
#' @export

plot_dml <- function(v, object, ..., xlim = NULL){

    dmls <- purrr::map(object$fit, ~ purrr::partial(univariateML::dml, obj = .x))

    plots <- purrr::map2(
        dmls, object$model,
        function(dfun, model) {
            plot_continuous(v, fun = dfun, ..., xlim = xlim) +
                ggplot2::labs(subtitle = model, x = NULL, y = NULL)
        }
    )

    if (length(plots) == 1) {
        return(plots[[1]])
    } else {
        return(plots)
    }
}

#' @rdname plot_models
#' @export

plot_pml <- function(v, object, ..., xlim = NULL){

    pmls <- purrr::map(object$fit, ~ purrr::partial(univariateML::pml, obj = .x))

    plots <- purrr::map2(
        pmls, object$model,
        function(pfun, model) {
            ggplot2::ggplot() +
                ggplot2::stat_ecdf(ggplot2::aes(x = v), ...) +
                ggplot2::stat_function(fun = pfun, n = 5e3, xlim = xlim) +
                ggplot2::labs(subtitle = model, x = NULL, y = NULL)
        }
    )

    if (length(plots) == 1) {
        return(plots[[1]])
    } else {
        return(plots)
    }
}

#' @rdname plot_models
#' @export

plot_qqml <- function(v, object, ...){

    qmls <- purrr::map(object$fit, ~ purrr::partial(univariateML::qml, obj = .x))

    plots <- purrr::map2(
        qmls, object$model,
        function(qfun, model) {
            ggplot2::ggplot() +
                ggplot2::stat_qq_line(ggplot2::aes(sample = v), distribution = qfun) +
                ggplot2::stat_qq(ggplot2::aes(sample = v), distribution = qfun, ...) +
                ggplot2::labs(subtitle = model, x = NULL, y = NULL)
        }
    )

    if (length(plots) == 1) {
        return(plots[[1]])
    } else {
        return(plots)
    }
}
