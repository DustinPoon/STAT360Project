#' Plot a MARS Model
#'
#' @description Plot of the fitted basis functions.
#'
#' @param x MARS model object (input mars object)
#' @param ... further arguments passed to or from other methods
#'
#' @usage \method{plot}{mars}(x, ...)
#'
#' @return 2D plot for single variable basis function
#'         3D plot for double variable basis function
#' @importFrom graphics par persp plot
#' @importFrom stats model.frame model.matrix terms
#' @examples
#' fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
#' plot.mars(fit.mars)
#' @rdname plot.mars
#' @export plot.mars
#' @export
#'
#' @seealso [mars()]
#' @seealso [print.mars()]
#' @seealso [summary.mars()]
#' @seealso [predict.mars()]
#' @seealso [anova.mars()]
plot.mars <- function(x, ...){
  # Retrieve the original dataset
  newdata<-eval(x$call$data)

  # Lab 10 Codes
  tt<-terms(x$formula,data=newdata)
  tt<-delete.response(tt)
  mf<-model.frame(tt,newdata)
  mt<-attr(mf,"terms")

  # Remove intercept
  X<-model.matrix(mt,mf)[,-1]

  Bf<-x$Bfuncs

  # Find indices of basis functions with single and double variables
  B_single<-which(sapply(Bf,function(x) NROW(x)==1))
  B_double<-which(sapply(Bf,function(x) NROW(x)==2))
  nn<-ceiling(sqrt(length(B_single)+length(B_double)))

  opar<-graphics::par(mfrow=c(nn,3),mar=c(2,2,2,2))
  # Exit handler to reset pars
  on.exit(graphics::par(opar))

  # Loop for single variable basis function
  for(i in B_single){
    var_ind <- Bf[[i]][1,"v"]

    x_coord <- seq(from=min(X[,var_ind]),to=max(X[,var_ind]),length=100)
    y_coord <- h(x_coord,Bf[[i]][1,"s"],Bf[[i]][1,"t"])

    # Plot line graph
    plot(x_coord,y_coord,type="l",
         xlab = x$x_names[var_ind],
         main = paste0(i-1, ". ", names(x$coefficients[i])))
    }

  # Loop for double variable basis function
  for(i in B_double){
    var1_ind <- Bf[[i]][1,"v"]
    var2_ind <- Bf[[i]][2,"v"]
    var1_name <- x$x_names[[var1_ind]]
    var2_name <- x$x_names[[var2_ind]]

    x_coord <- seq(from=min(X[,var1_ind]),to=max(X[,var1_ind]),length=100)
    y_coord <- seq(from=min(X[,var2_ind]),to=max(X[,var2_ind]),length=100)

    basis_fun <- function(x, y){
      h(x, Bf[[i]][1,"s"], Bf[[i]][1, "t"]) * h(y, Bf[[i]][1,"s"], Bf[[i]][1,"t"])}

    z_coord <- outer(x_coord, y_coord, FUN = basis_fun)
    # Plot 3d graph
    persp(x_coord,y_coord,z_coord,
          xlab=var1_name,ylab=var2_name,zlab="",
          main=paste0(i-1, ". ", var1_name,":",var2_name, " (", names(x$coefficients[i]),
                      ")"),theta=-25,phi=25,col="lightblue",lwd=.1)
  }
  opar<-graphics::par(mfrow=c(nn,nn),mar=c(2,2,2,2))
  # Exit handler to reset pars
  on.exit(graphics::par(opar))
}
