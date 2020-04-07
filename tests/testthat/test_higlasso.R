context("Test higlaso")

test_that("cv.higlasso works", {


X <- as.matrix(higlasso.df[1:400, paste0("V", 1:7)])
Y <- higlasso.df$Y[1:400]
Z <- matrix(1, nrow(X))


expect_silent({
    higlasso.fit <- cv.higlasso(Y, X, Z, nfolds = 3, nlambda1 = 5, nlambda2 = 5,
                                degree = 2)
})
})
