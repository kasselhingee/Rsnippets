% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotationmatrix.R
\name{rotationmatrix}
\alias{rotationmatrix}
\title{Rotation Matrix From Two Vectors}
\usage{
rotationmatrix(a, b)
}
\arguments{
\item{a}{The vector to rotate.}

\item{b}{The final vector.}
}
\description{
Creates a rotation matrix that rotates between two vectors. The matrix was specified by \insertCite{@Section 3.2.1, @amaral2007pi;textual}{Rsnippets}, and is such that any vector perpendicular to the two vectors is unchanged (except when the two vectors are in exactly opposite directions).
}
\details{
The return matrix \eqn{Q} is a rotation such that \eqn{Qa=b}, and for any vector \eqn{z} perpendicular to both \code{b} and \code{a}, \eqn{Qz=z}.
In the extremely rare situation that \code{b} = -\code{a}, the \insertCite{amaral2007pi;textual}{Rsnippets} method does not apply. Instead \code{rotationmatrix()}, rotates \code{a} to the south pole, applies a rotation of \code{pi} that passes through the second basis vector and then reverses the first rotation.
The same method, without the case of \code{b = -a}, is also implemented in \code{rotation()} function of the \href{https://cran.r-project.org/package=Directional}{\code{Directional}} package.

If the angle between the two vectors is smaller than \code{sqrt(.Machine$double.eps)} then the identity matrix is returned.
}
\examples{
a <- c(1,2,3,4,5)
b <- c(0,3,4,1,2)
a <- a / sqrt(sum(a^2))
b <- b /sqrt(sum(b^2))
all.equal(rotationmatrix(a, b) \%*\% a, b)
}
\references{
\insertAllCited{}
}
