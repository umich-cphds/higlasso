# higlasso

## Installation
`higlasso` can be installed via Github using `devtools`

```r
# install.packages("devtools")
devtools::install_github("umich-cphds/higlasso")
```
You'll need a working C++11 compiler, which can obtained by installing Xcode on
MacOS, and RTools on Windows.
## Example
This is a very rough example on how to tune higlasso
 ```{r}
library(higlasso)

X <- higlasso.df[, paste0("X", 1:10)]
Y <- higlasso.df$y
generate.Xm <- function(i, degree)
{
    m <- splines::bs(X[, i], degree = degree)
    m <- qr.Q(qr(m))
    apply(m, 2, function(x) x / stats::sd(x))
}

Xm <- lapply(1:ncol(X), generate.Xm, degree = 3)
Xm.train <- lapply(Xm, function(xm) xm[1:400,])
Xm.test <- lapply(Xm, function(xm) xm[401:500,])

Y.train <- Y[1:400]
Y.test  <- Y[401:500]

Z.train <- matrix(1, 400)
Z.test  <- matrix(1, 100)


l1 <- 1:10/10
l2 <- 1:10/10

best.mspe   <- 999
best.lambda <- c(999, 999)

for (lambda1 in l1) {
    for (lambda2 in l2) {

        higlasso.out <- higlasso(Y.train, Z = Z.train,
            Xm = Xm.train, lambda1 = lambda1, lambda2 = lambda2,
            maxit = 10000)
        mspe.test <- predict(higlasso.out, list(Y.test, Xm.test, Z.test))
        if (mspe.test < best.mspe) {
            best.mspe <- mspe.test
            best.lambda = c(lambda1, lambda2)
        }

    }
}

best <- higlasso(Y.train, Z = Z.train, Xm = Xm.train,
    lambda1 = best.lambda[1], lambda2 = best.lambda[2])
best
```
