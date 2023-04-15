#' Anova for MARS
#'
#' @param object An object of class mars.
#' @param ... Other arguments.
#' @return The anova table of the fitted MARS model.
#'
#' @export

anova.mars <- function(object, ...) {
  anova(lm(y ~ ., data = data.frame(y = object$y, object$B)))
}
