\name{gsubfn-package}
\alias{gsubfn-package}
\docType{package}
\title{
gsubfn
}
\description{
Generalized \code{"'gsub'"} and associated function.
}
\details{
\code{"'gsubfn'"} is like \code{"'gsub'"} except 
instead of using a replacement string it passes
the matches and backreferences to a function
which transforms them and then replaces the matches
with that.  Some applications include
perl-style string interpolation, splitting a string
based on matches rather than delimiters (using
the function \code{"'strapply'"}) and other
string transformation applications.

Index:
\preformatted{
gsubfn                  Pattern Matching and Replacement
strapply                Apply a function over a string or strings.
}
}
\keyword{ package }
\examples{

# replace each number with that number plus 1
gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 

# perl-style string interpolation
gsubfn(,, "pi = $pi\n")

# split out numbers
strapply("12abc34 55", "[0-9]+")

}
