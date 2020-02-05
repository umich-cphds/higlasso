#' Synthetic Example Data For Higlasso
#'
#' This synthetic data is taken from the linear interaction simulations from
#' the higlasso paper. The data generating model is:
#' \deqn{Y = X_1 + X_2 + X_3 + X_4 + X_5 + X_1 X_2 + X_1 X_3 + X_2 X_3}
#' \deqn{ + X_1 X_4 + X_2 X_4 + X_3 X_4 + X_1 X_5}
#' \deqn{+ X_2 X_5 + X_3 X_5 + X_4 X_5 + \epsilon}
#' @format A data.frame with 1000 observations on 11 variables:
#' \describe{
#'   \item{Y}{Continuous response.}
#'   \item{X1-X10}{Covariates.}
#' }
"higlasso.df"
