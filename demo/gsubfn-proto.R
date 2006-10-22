
"?" <- function(...) invisible(0)

library(proto)

?"replace second occurrence of - with +"
?"uses builtin count variable"
p <- fn$proto(fun = ... ~ { if (count == 2) "+" else "-" })
gsubfn("-", p, c("0-2-1", "9-7-3"))

?"return match except for second match return X"
p <- fn$proto(fun = this + x ~ { if (count == 2) "X" else x })
strapply("the big green house", "\\w+", p)

?"successive pairs of words separated with -"
pairs <- proto(
	pre = function(this) { this$last <- NULL },
	fun = function(this, x) {
	   out <- if (is.null(this[["last"]])) "" else paste(last, x, sep = "-")
	   this$last <- x
	   out
        }
)
gsubfn("\\w+", pairs, " a b c d e f", perl = TRUE)

?"matrix of succesive word pairs"
pairs2 <- proto(
	pre = function(this) { this$last <- NULL },
	fun = function(this, x) {
	   out <- if (!is.null(this[["last"]])) c(last, x)
	   this$last <- x
	   out
        }
)

matrix(strapply(" a b c d e f", "\\w+", pairs2)[[1]], 2)

?"count repetitions of each word"
?"words[[x]] contains the number of repetitions of word x"
?"output has each word suffixed with its count so far"
words <- proto(
	pre = function(this) { this$words <- list() },
	fun = function(this, x) {
		if (is.null(words[[x]])) words[[x]] <<- 0
		words[[x]] <<- words[[x]] + 1
		paste0(x, "{", words[[x]], "}")
	}
)

gsubfn("\\w+", words, "the dog and the cat are in the house")


?"parenthesis matching"
?"top of stack contains number of most recent nonclosed open paren"
?"open is the number of open parens so far regardless of whether closed or not"
?"output has each paren suffixed with its number"
paren <- proto(
	pre = function(this) { this$stack <- c(); this$open <- 0 },
	fun = function(this, x) { 
		if (x == "(") { current <- open <<- open + 1
			stack <<- c(stack, open)
		} else {
			current <- tail(stack, 1)
			stack <<- head(stack, -1)
		}
		paste0(x, "{", current, "}")
	})
gsubfn("[()]", paren, c("((a+(b)) + c+(d))", "((hello)(world))"))

rm("?")

