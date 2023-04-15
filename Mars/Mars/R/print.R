#' Print method for MARS
#'
#' @description Print coefficients of the fitted mars model.
#'
#' @param object an object of class 'mars'.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Vector of coefficients.
#' @export
#'
#' @examples
#' @import stats
#' @import ISLR
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' print(fit.mars)
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
