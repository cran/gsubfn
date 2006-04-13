
# supports function as replacement argument.  Matched string is passed to
# function as arg1, with subsequent args being the backreferences.  
# Backref is number of backrefs that are passed to function and is normally 
# left at default value although it can be set lower for improved efficiency, 
# e.g. backref = 0 if no backreferences are to be passed.
#
# e.g. gsubfn("[[:digit:]]+", function(x) as.numeric(x)+1, "(10 20)(100 30)") 
#   adds 1 to each number in third arg
#
# e.g. f <- function(x,y,z) as.numeric(y)+as.numeric(z),
#      gsubfn("([0-9]+):([0-9]+)", f, "abc 10:20 def 30:40 50")
#   replaces pairs m:n with their sum
#
# e.g. gsubfn( , , "pi = $pi, 2pi = `2*pi`") 
#
# e.g. v <- c(); f <- function(x) v <<- append(v,as.numeric(x))
#      gsubfn("[0-9]+", f, "12;34:56,89,,12")
#   extracts numbers from string and places them into vector v
#
# e.g. gsubfn("\\B.", tolower, "I LIKE A BANANA SPLIT")
#   makes all letters except first in word lower case
#
gsubfn <- function(pattern = "[$]([[:alpha:]][[:alnum:].]*)|`([^`]+)`",
	  replacement = function(x,b1,b2) eval(parse(text = paste(b1,b2,sep="")), env), 
          x, backref = nchar(base::gsub("[^(]","",pattern)), 
	  env = parent.frame(), ...) 
{
   if (is.character(replacement)) 
		return(base::gsub(pattern, replacement, x, ...))
   stopifnot(is.character(pattern), is.character(x), is.function(replacement))
   force(env)
   gsub.function <- function(x) {
      x <- base::gsub('"', '\\\\"', x)
      pattern <- paste("(", pattern, ")", sep = "")
      repl <- function(i) {  
	      rs <- paste('"\\', seq(i+1), '"', collapse = ",", sep = "") 
	      rs <- paste('",replacement(', rs, '),"', sep = "")
              # if backref= is too large, reduce by 1 and try again
	      tryCatch(base::gsub(pattern, rs, x, ext = TRUE),
			error = function(x) if (i > 0) repl(i-1) else stop(x))
      }
      z <- repl(backref+1)
      z <- paste('c("', z, '")', sep = "")
      paste(eval(parse(text = z)), collapse = "")
   }
   sapply(x, gsub.function)
}

