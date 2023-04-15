#' Summary method for MARS
#'
#' @description summary method for class 'mars'.
#' 
#' @usage summary(object,...)
#'
#' @param object an object of class 'mars'.
#' @param ... other arguments -- currently not used.
#'
#' @return Print the summary of the mars object and generic function with mars object.
#' @examples
#' @import stats
#' @import ISLR
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' summary(fit.mars)
#' @rdname summary.mars
#' @export summary.mars
#' @export

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
