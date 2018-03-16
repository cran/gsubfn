\name{transform2}
\alias{transform2}
\title{
Like transform but allows right hand sides to refer to left hand sides.
}
\description{
In \code{transform} the right hand sides of the arguments may not refer
to the left hand sides.  \code{transform2} works like \code{transform}
with the exception that a right hand side may refer to any of the variables
on the left hand side of another argument.  The argument defining the 
left hand side may come before or after the reference.
}
\usage{
transform2(`_data`, ...)
}
\arguments{
  \item{_data}{
	  A data frame.
}
  \item{\dots}{
	  Further arguments of the form \code{tag=value}.
}
}

\value{
	The modified value of \code{_data}.
}
\seealso{
	\code{\link[base]{transform}},
	\code{\link[base]{within}}
}
\examples{
\dontrun{
# bad
transform(BOD, b = a, a = demand)
}
# OK.  a is set to demand and b is set to a so both wind up equal to demand
transform2(BOD, b = a, a = demand)
}
\keyword{misc}

