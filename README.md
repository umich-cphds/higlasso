higlasso
========

Installation
------------

`higlasso` can be installed via Github using `devtools`

    # install.packages("devtools")
    devtools::install_github("umich-cphds/higlasso")

You'll need a working C++11 compiler, which can obtained by installing
Xcode on MacOS, and RTools on Windows. \#\# Example

This is a very rough example on how to run higlasso

    library(higlasso)

    X <- higlasso.df[, paste0("X", 1:10)]
    Y <- higlasso.df$y

    Y.train <- Y[1:400]
    X.train <- as.matrix(X[1:400,])
    Z.train <- matrix(1, 400)

    X.test <- as.matrix(X[401:500,])
    Y.test  <- Y[401:500]
    Z.test  <- matrix(1, 100)

    higlass.out <- higlasso(Y.train, X.train, Z.train, Y.test = Y.test,
                            X.test = X.test, Z.test = Z.test)
