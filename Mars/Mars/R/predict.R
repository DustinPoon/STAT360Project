#' Predict method for MARS
#'
#' @description Predicted values of fitted mars model.
#' 
#' @usage predict(object, newdata)
#'
#' @param object an object of class 'mars'.
#' @param newdata an optional data frame in which to look for variables with which to predict.
#'
#' @return predicted a vector of predictions.
#'
#' @examples
#' @import stats
#' @import ISLR
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' predict(fit.mars)
#' @rdname predict.mars
#' @export predict.mars
#' @export
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
