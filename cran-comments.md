This is a re-submission.

## Test environments
* Ubuntu 16.04 (travis): r-devel (2020-05-11 r78411), r-release (4.0.0), r-oldrelease (3.6.2)

* winbuilder: r-devel (2020-05-11 r78411), r-release (4.0.0)

## R CMD check results
> There were no ERRORs or WARNINGs.

> There was 1 NOTE:

> Maintainer: ‘Alexander Rix <alexrix@umich.edu>’

> New submission

## Reviewer comments

> Flavor: r-devel-windows-ix86+x86_64
> Check: Overall checktime, Result: NOTE
>    Overall checktime 12 min > 10 min
>
> A CRAN package check should not take longer than 10 minutes. Please
> considerably reduce the check time of your package to stay below the
> threshold of 10 minutes... This can for example be achieved by:

I have modified the package and now the checktime is
Installation time in seconds: 76
Check time in seconds: 309

* running tests for arch 'i386' ... [73s] OK
  Running 'testthat.R' [73s]
** running tests for arch 'x64' ... [86s] OK
  Running 'testthat.R' [86s]
* checking re-building of vignette outputs ... [73s] OK

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form...

I have added a link to the arXic pre-print.

> \dontrun{} should only be used if the example really cannot be executed
> (e.g. because of missing additional software, missing API keys, ...) by
> the user. That's why wrapping examples in \dontrun{} adds the comment
> ("# Not run:") as a warning for the user.
> Does not seem necessary.
> Please replace \dontrun with \donttest.

Done.

> Please do not modifiy the .GlobalEnv. This is not allowed by the CRAN
> policies.
>
> Please do not modify the global environment (e.g. by using <<-) in your
> functions. This is not allowed by the CRAN policies.

I have removed the `<<-`. I don't think there are any other modifications of
`.GlobalEnv` in the code.

## Previous Reviewer comments

> Please always explain all acronyms/abbreviations in the description text
> in the Description field of the DESCRIPTION file.

LASSO has been replaced by Least Absolute Shrinkage and Selection Operator.

> Please add \value to all .Rd files for exported functions and explain
> the functions results in the documentation.
> f.i.: print.cv.higlasso.Rd
> If a function does not return a value, please document that too, e.g.
> \value{None}.

print.cv.higlasso now has a value

> ** running tests for arch 'i386' ... [532s] OK
> ** running tests for arch 'x64' ... [482s] OK
> ** checking re-building of vignette outputs ... [278s] OK

> This is much more than the CRAN threshold oif 10 min for a package
> check, so please reduce by using toy data/few iterations and perhaps run
> less important tests only locally.

This has been reduced (according to winbuilder) to
** running tests for arch 'i386' ... [41s] OK
** running tests for arch 'x64' ... [44s] OK
** checking re-building of vignette outputs ... [251s] OK
