#' Anova method for MARS
#'
#' @description Compute an analysis of variance table for the fitted MARS model.
#'
#' @usage anova(object,...)
#'
#' @param object an object of class 'mars'.
#' @param ... other arguments -- currently not used.
#'
#' @return An object of class 'anova' inheriting from class 'data.frame'.
#' @export
#'
#' @examples
#' @import stats
#' @import ISLR
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' anova(fit.mars)
#'
#' @rdname anova.mars
#' @export anova.mars
#' @export
anova.mars <- function(object, ...) {
  anova(lm(y ~ ., data = data.frame(y = object$y, object$B)))
}
