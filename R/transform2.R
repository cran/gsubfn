
transform2 <- function(`_data`, ...) {
	f <- function(){}
	formals(f) <- eval(substitute(as.pairlist(c(alist(...), `_data`))))
	body(f) <- substitute(modifyList(`_data`, data.frame(...)))
	f()
}
