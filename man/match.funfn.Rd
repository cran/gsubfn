\name{match.funfn}
\alias{match.funfn}
\title{ Generic extended version of R match.fun }
\description{
  A generic \code{match.fun}.
}
\usage{
match.funfn(FUN, descend = TRUE)
}
\arguments{
  \item{FUN}{ Function, character name of function or formula describing 
  function. }
  \item{descend}{logical; control whether to search past non-function objects.}
}
\details{
  The default method is the same as \code{match.fun} and the \code{formula}
  method is the same as \code{as.function.formula}.  This function can
  be used within the body of a function to convert a function specification
  whether its a function, character string or formula into an actual function.
}
\value{
  Returns a function.
}
\seealso{ See Also \code{\link{match.fun}}, 
\code{\link{as.function.formula}}.}
\examples{
# return first argument evaluated at second argument squared.
sq <- function(f, x) {
   f <- match.funfn(f)
   f(x^2)
}

# call sq using different forms for function
sq(function(x) exp(x)/x, pi)
f <- function(x) exp(x)/x
sq("f", pi)
sq(~ exp(x)/x, pi)
sq(x ~ exp(x)/x, pi)

}
\keyword{ programming }
