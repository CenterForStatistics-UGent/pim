#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy of a pim object
#'
#' @param x An `pim` object created by \code{\link[pim]{pim}}.
#' @param conf.int Logical indicating whether or not to include
#'   a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence
#'   interval if conf.int = TRUE. Must be strictly greater than 0
#'   and less than 1. Defaults to 0.95, which corresponds to a
#'   95 percent confidence interval
#' @param expit Logical indicating whether or not to apply \code{\link[stats]{plogis}} to the
#' coefficient estimates. Defaults to FALSE.
#' @param ... Unused, included for generic consistency only.
#'
#' @return A tidy \code{\link[tibble]{tibble}} summarizing component-level
#'   information about the model
#'
#' @examplesIf rlang::is_installed("ggplot2")
#'
#' library(ggplot2)
#' library(dplyr)
#' library(pim)
#' data('FEVData')
#'
#' mod <- pim(FEV~ Smoke*Sex , data=FEVData)
#'
#' tidy(mod)
#'
#' # coefficient plot
#' d <- tidy(mod, conf.int = TRUE)
#'
#' ggplot(d, aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
#'   geom_point() +
#'   geom_vline(xintercept = 0, lty = 4) +
#'   geom_errorbarh()
#'
#' # probabilistic index plot
#' d <- tidy(mod, conf.int = TRUE, expit = TRUE)
#'
#' ggplot(d, aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
#'   geom_point() +
#'   geom_vline(xintercept = 0, lty = 4) +
#'   geom_errorbarh()
#'
#' @aliases pim_tidiers
#' @seealso \code{\link[broom]{tidy}}, \code{\link[pim]{summary.pim}}
#' @family pim tidiers
#' @export
tidy.pim <- function(x, conf.int = FALSE, conf.level = 0.95,
                     expit = FALSE, ...) {
  s <- pim::summary(x)
  ret <- tibble(term = names(s@coef),
                estimate = s@coef,
                std.error = s@se,
                statistic = s@zval,
                p.value = s@pr)

  if (conf.int) {
    ci <- confint(x, level = conf.level) %>%
      tibble::as_tibble(rownames = "term") |>
      rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }

  if (expit) {
    ret <- plogis(ret)
  }

  ret
}
