#' Print method for MARS Model
#'
#' @description Print coefficients of the fitted mars model.
#'
#' @param x MARS model object
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' print.mars(fit.mars)
#'
#' @rdname print.mars
#' @export print.mars
#' @export
print.mars <- function(x, ...){
  cat("\n Call: \n")
  print.default(x$call)

  cat("\n Coefficients: \n")
  print.default(x$coefficients)
}
