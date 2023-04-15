#' Prediction Function for MARS Model
#'
#' @description Predicted values of fitted mars model.
#'
#' @param object MARS model object (input mars object)
#' @param newdata data frame or matrix with new data
#' @param ... further arguments to be passed to or from methods.
#'
#' @usage \method{predict}{mars}(object, newdata, ...)
#'
#' @return predicted values of fitted model
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' predict.mars(fit.mars)
#' @rdname predict.mars
#' @export predict.mars
#' @export
#'
#' @seealso [mars()]
#' @seealso [print.mars()]
#' @seealso [summary.mars()]
#' @seealso [plot.mars()]
#' @seealso [anova.mars()]
predict.mars <- function(object,newdata,...) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

make_B <- function(X, Bfuncs){
  n <- length(Bfuncs); out <- init_B(nrow(X), n-1)
  for(i in 2:n){
    temp <- 1
    for(j in 1:nrow(Bfuncs[[i]])){
      temp <- temp * h(X[,Bfuncs[[i]][j,"v"]], Bfuncs[[i]][j,"s"], Bfuncs[[i]][j,"t"])
    }
    out[,i] <- temp
  }
  return(as.matrix(out))
}
