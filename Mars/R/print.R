#' Print method for MARS Model
#'
#' @description Print coefficients of the fitted mars model.
#'
#' @usage \method{print}{mars}(x, ...)
#'
#' @param x MARS model object (input mars object)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' print.mars(fit.mars)
#'
#' #example 2
#' # create your own dataset
#' n <- 1000
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' y <- 2*x1 + 3*x2 + 4*x3 + rnorm(n)
#' df <- data.frame(x1=x1,x2=x2,x3=x3,y=y)
#'
#' # fit a MARS model
#' fit <- mars(y ~ x1 + x2 + x3, data=df)
#' #show results
#' print.mars(fit)
#'
#' @seealso [mars()]
#' @seealso [summary.mars()]
#' @seealso [predict.mars()]
#' @seealso [plot.mars()]
#' @seealso [anova.mars()]
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
