# ADMUR 1.0.0

## 20-11-06 
Initial submission to CRAN

## 20-11-11 
CRAN request to fix and resubmit the following from Gregor Seyer:

\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.

You are setting options(warn=-1) in your function. This is not allowed. Please rather use suppressWarnings() if really needed.

Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
par(mfrow=c(2,2))            # somewhere after
e.g.: functions.R
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.

Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos.
e.g.: inst/doc/guide.R
oldpar <- par(mfrow = c(1,2))
par(oldpar)

