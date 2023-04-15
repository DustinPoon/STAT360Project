#' Anova for MARS
#'
#' @param object An object of class mars.
#' @param ... Other arguments.
#' @return The anova table of the fitted MARS model.
#'
#'
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' anova.mars(fit.mars)
#' @import stats
#' @import ISLR
#'
#' @rdname anova.mars
#' @export anova.mars
#' @export
#'
anova.mars <- function(object, ...) {
  anova(lm(y ~ ., data = data.frame(y = object$y, object$B)))
}
