## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  mars(formula, data, control = NULL)

## ----eval=FALSE---------------------------------------------------------------
#  library(ISLR)
#  data(Wage)
#  data(iris)
#  CarPrice <- read.csv("CarPrice_Assignment.csv")

## ----eval=FALSE---------------------------------------------------------------
#  mc <- mars.control(Mmax=10)

## ----eval=FALSE---------------------------------------------------------------
#  fit.Wage <- mars(wage ~ age + education, Wage, mc)
#  print.mars(fit.Wage)
#  predict.mars(fit.Wage, newdata = data.frame(age=Wage$age,
#                                              education = Wage$education))
#  summary.mars(fit.Wage)
#  plot.mars(fit.Wage)
#  anova.mars(fit.Wage)

## ----eval=FALSE---------------------------------------------------------------
#  fit.iris <- mars(Sepal.Length ~., iris, mc)
#  print.mars(fit.iris)
#  predict.mars(fit.iris, newdata = data.frame(sepal.width = iris$Sepal.Width,
#                                              petal.length = iris$Petal.Length,
#                                              petal.width = iris$Petal.Width,
#                                              Species = iris$Species))
#  summary.mars(fit.iris)
#  plot.mars(fit.iris)
#  anova.mars(fit.iris)

## ----eval=FALSE---------------------------------------------------------------
#  fit.CarPrice <- mars(price ~ citympg + highwaympg + horsepower + fueltype, CarPrice, mc)
#  print.mars(fit.CarPrice)
#  predict.mars(fit.CarPrice, newdata = data.frame(citympg = CarPrice$citympg,
#                                                  highwaympg = CarPrice$highwaympg,
#                                                  horsepower = CarPrice$highwaympg,
#                                                  fueltype = CarPrice$highwaympg))
#  summary.mars(fit.CarPrice)
#  plot.mars(fit.CarPrice)
#  anova.mars(fit.CarPrice)

