#' Summary Function for MARS Model
#'
#' @description summary method for class 'mars'.
#'
#' @param object  MARS model object (input mars object)
#' @param ... further arguments passed to or from other methods
#'
#' @usage \method{summary}{mars}(object,...)
#'
#' @return The function return the summary of the input MARS object as well as
#' a generic summary function with the MARS object
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' summary.mars(fit.mars)
#' @rdname summary.mars
#' @export summary.mars
#' @export
#'
#' @seealso [mars()]
#' @seealso [print.mars()]
#' @seealso [predict.mars()]
#' @seealso [plot.mars()]
#' @seealso [anova.mars()]
summary.mars <- function(object, ...){
  cat("\nB0: Intercept\n")

  for (i in 2:length(object$Bfuncs)){
    cat(paste0("\n", names(object$coefficient)[i], ": "))
    for (j in 1:nrow(object$Bfuncs[[i]])){
      cat(paste0("Sign: ", object$Bfuncs[[i]][j,"s"], "\n"))
      cat(paste0("Split Variables: ", object$Bfuncs[[i]][j,"v"], "\n"))
      cat(paste0("Split Points: ", object$Bfuncs[[i]][j,"t"], "\n"))
    }
  }
  summary.lm(object)
}
