% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catchew.R
\name{catchew}
\alias{catchew}
\title{Catch errors, warnings, stderr and put all into a text object}
\usage{
catchew(expr)
}
\description{
Another package redefines a function, which might be the best way to do it. I find this way simpler at the moment. I find this function useful for simulation studies.
}
\details{
Much of information about capturing the warnings I got from \link{http://adv-r.had.co.nz/beyond-exception-handling.html}
}
\examples{
catchew({warning("a test warning"); "hello"})
catchew({stop("a test stop"); "hello"})
}
