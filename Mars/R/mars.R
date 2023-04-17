#' Multivariate Adaptive Regression Splines (MARS)
#'
#' @description Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @aliases mars
#' @usage mars(formula, data, control) #for generating a mars object
#' @details
#' The function first extracts the response variable and the predictor variables
#'  from the data frame using the formula and `model.frame() function`. Then, it performs
#'  forward stepwise regression on the data using the `fwd_stepwise()` function, followed by
#'  backward stepwise regression on the selected terms using the `bwd_stepwise()` function.
#'  The resulting basis functions and basis matrix are stored in the bwd object. Finally, a
#'  linear model is fit to the data using the `lm()` function, with the basis matrix and response
#'  variable as inputs. The resulting object is returned as a list with class 'mars'.
#'
#' @param formula an R formula specifying the dependent and independent variables in the model. The formula should take the form y ~ x1 + x2 + ..., where y is the response variable and x1, x2, etc. are the predictor variables.
#' @param data a data frame containing the variables in the formula.
#' @param control an object of class mars.control that specifies parameters used in the model fitting procedure. By default, the control object is constructed using `mars.control()`, with default values for the parameters.
#'
#' @return an object of class 'mars' which will be used for plot, predict, etc.
#' @export
#' @examples
#' #example 1
#' # mars() with ISLR::Wage dataset
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' # Show results
#' fit.mars
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
#' fit
#'
#' #example 3
#' # another example using data(iris)
#' fit.iris <- mars(Sepal.Length ~., data=iris, control = mars.control(Mmax=10))
#' # Show results
#' fit.iris
#'
#'
#' @import stats
#' @import ISLR
#'
#' @seealso [print.mars()]
#' @seealso [summary.mars()]
#' @seealso [predict.mars()]
#' @seealso [plot.mars()]
#' @seealso [anova.mars()]
#'
#' @references
#' Friedman, J. H. (1991). Multivariate adaptive regression splines. The Annals of Statistics, 19(1), 1-67.
#'
#' @author Group DJT: Dustin Poon, James Lee, Tyler Oh
# ....
# .....
mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  # Fitting model with bwd and dropping intercept to overcome colinearity due to presence of two intercepts
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B))
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}

#------------------- fwd_stepwise ----------------------
fwd_stepwise <- function(y,x,mc=mars.control()){
  # Initializing
  N <- length(y) # Sample Size
  n <- ncol(x)   # Number of predictors
  B <- init_B(N, mc$Mmax)
  Bfuncs <- vector(mode = "list",length = mc$Mmax+1)
  # Forward Selection
  for(i in 1:(mc$Mmax/2)) {
    M <- 2*i-1
    lof_best <- Inf
    # Use setdiff to find set of variables
    for(m in 1:M) {
      set_vars <- setdiff(1:n,Bfuncs[[m]][,'v'])
      for(v in set_vars){
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,1:M],
                             Btem1=B[,m]*h(x[,v],+1,t),
                             Btem2=B[,m]*h(x[,v],-1,t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat,mc)
          if(lof < lof_best) {
            lof_best <- lof
            split_best <- c(m=m,v=v,t=t)
          }#lof
        }# end loop over splits
      }# end loop over variables
    }#m loop
    m <- split_best['m']; v <- split_best['v']; t <- split_best['t']
    #cat("best split on variable",v, "at", t, "\n")
    Bfuncs[[M+1]] <- rbind(Bfuncs[[m]],c(s=-1,v,t))
    Bfuncs[[M+2]] <- rbind(Bfuncs[[m]],c(s=+1,v,t))
    B[,M+1] <- B[,m]*h(x[,v],-1,t)
    B[,M+2] <- B[,m]*h(x[,v],+1,t)
  }
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}

#------------------------ init_B -------------------------
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=(Mmax+1)))
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

#--------------------- bwd_stepwise ----------------------
bwd_stepwise <- function(fwd,mc){
  Mmax <- ncol(fwd$B)-1
  Jstar <- 2:(Mmax+1)
  Kstar <- Jstar
  dat <- data.frame(y=fwd$y,fwd$B)
  lofstar <- LOF(y~.-1,dat,mc)
  for (M in (ncol(fwd$B)):2 ){
    b <- Inf
    L <- Kstar
    for (m in L){
      K <- setdiff(L,m)
      dat <- data.frame(y=fwd$y,fwd$B[,K])
      lof <- LOF(y~.,dat,mc)
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
  }
  Jstar <- c(1,Jstar)
  B <- fwd$B[,Jstar]
  Bfuncs <- fwd$Bfuncs[Jstar]
  return(list(y=fwd$y,B=B,Bfuncs=Bfuncs))
}

#------------------------ LOF ----------------------------
LOF <- function(formula,data,mc) {

  fit.lm <- lm(formula, data)

  RSS <- sum(residuals(fit.lm)^2)
  N <- nrow(data)
  M <- length(coef(fit.lm))-1
  D <- mc$d
  CM <- sum(hatvalues(fit.lm))

  value <- RSS*(N/((N-(CM+D*M))^2))
  return(value)
}

#---------------------- hinge function -------------------
h <- function(x,s,t){
  return(pmax(0, s*(x-t)))
}

split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

#----------------------------------------------------------#
# constructor, validator and helper for class mars.control #
#----------------------------------------------------------#

new_mars.control <- function(control) {
  structure(control,class="mars.control")
}

validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control
}

#' mars.control
#'
#' @param Mmax an integer representing maximum number of basis functions allowed by the user, default value is 2
#' @param d smoothing parameter
#' @param trace logical value. if user wants to see what is happening in the mars function, then set it to TRUE. Default value is FALSE
#'
#' @return an empty list
#' @export
#'
#' @examples
#' # example 1
#' mc <- mars.control(Mmax=10)
#' mc
#'
#' # example 2
#' # when Mmax is not even
#' try(mars.control(Mmax=3))
#'
#' # example 3
#' # when Mmax is not even, it will round up
#' try(mars.control(Mmax=5))
#'
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}
