#' Anova for MARS
#'
#' @description Compute an analysis of variance table for the fitted MARS model.
#'
#' @param object An object of class mars.
#' @param ... Other arguments.
#' @return The anova table of the fitted MARS model.
#'
#' @aliases anova
#' @usage \method{anova}{mars}(object, ...)
#'
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' anova.mars(fit.mars)
#' @import stats
#' @import ISLR
#'
#' @seealso [mars()]
#' @seealso [print.mars()]
#' @seealso [summary.mars()]
#' @seealso [predict.mars()]
#' @seealso [plot.mars()]
#'
#' @rdname anova.mars
#' @export anova.mars
#' @export
#'
anova.mars <- function(object, ...) {
  anova(lm(y ~ ., data = data.frame(y = object$y, object$B)))
}
