## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Mars)

## ---- eval=FALSE--------------------------------------------------------------
#  mars(formula, data, control)

## -----------------------------------------------------------------------------
library(ISLR)
fit.mars <- mars(wage ~ age + education, data=ISLR::Wage, control = mars.control(Mmax=10))
fit.mars

## -----------------------------------------------------------------------------
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 2*x1 + 3*x2 + 4*x3 + rnorm(n)
df <- data.frame(x1=x1,x2=x2,x3=x3,y=y)

fit <- mars(y ~ x1 + x2 + x3, data=df)
fit

## -----------------------------------------------------------------------------
fit.iris <- mars(Sepal.Length ~., data=iris, control = mars.control(Mmax=10))
fit.iris


