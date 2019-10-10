higlasso
========

Installation
------------

`higlasso` can be installed via Github using `devtools`

    # install.packages("devtools")
    devtools::install_github("umich-cphds/higlasso")

You'll need a working C++11 compiler, which can obtained by installing
Xcode on MacOS, and RTools on Windows. \#\# Example This is a very rough
example on how to tune higlasso

\`\`\`r system.time({ library(higlasso)

X &lt;- higlasso.df\[, paste0("X", 1:10)\] Y &lt;- higlasso.df$y
generate.Xm &lt;- function(i, degree) { m &lt;- splines::bs(X\[, i\],
degree = degree) m &lt;- qr.Q(qr(m)) apply(m, 2, function(x) x /
stats::sd(x)) }

Xm &lt;- lapply(1:ncol(X), generate.Xm, degree = 3) Xm.train &lt;-
lapply(Xm, function(xm) xm\[1:400,\]) Xm.test &lt;- lapply(Xm,
function(xm) xm\[401:500,\])

Y.train &lt;- Y\[1:400\] Y.test &lt;- Y\[401:500\]

Z.train &lt;- matrix(1, 400) Z.test &lt;- matrix(1, 100)

l1 &lt;- 1:10/5 l2 &lt;- 1:10/5

best.mspe &lt;- 999 best.lambda &lt;- c(999, 999)

for (lambda1 in l1) { for (lambda2 in l2) { higlasso.out &lt;-
higlasso(Y.train, Z = Z.train, Xm = Xm.train, lambda1 = lambda1, lambda2
= lambda2, maxit = 5000) mspe.test &lt;- predict(higlasso.out,
list(Y.test, Xm.test, Z.test)) if (mspe.test &lt; best.mspe) { best.mspe
&lt;- mspe.test best.lambda = c(lambda1, lambda2) } } } best &lt;-
higlasso(Y.train, Z = Z.train, Xm = Xm.train, lambda1 =
best.lambda\[1\], lambda2 = best.lambda\[2\], sigma = 1) }) \`\`\`


`##    user  system elapsed   ## 637.775  18.572 656.394`

`r  best`

`## HiGLASSO fit  ## Selected main effects:  ##  G1  G2  G3  G4  G5  G6  G7  G8  G9 G10   ##   1   1                                   ## Selected interaction effects:  ##     G1 G2 G3 G4 G5 G6 G7 G8 G9 G10  ## G1                                  ## G2   1                              ## G3                                  ## G4                                  ## G5                                  ## G6                                  ## G7                                  ## G8                                  ## G9                                  ## G10                                 ## Mean squared prediction error: 0.896579  ## Lambda = 0.2 2`
