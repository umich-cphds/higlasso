context("Test higlaso")

test_that("cv.higlasso works", {


X <- as.matrix(higlasso.df[, paste0("V", 1:10)])
Y <- higlasso.df$Y
Z <- matrix(1, nrow(X))


expect_silent({
    higlasso.fit <- cv.higlasso(Y, X, Z, nfolds = 5)
})
})
