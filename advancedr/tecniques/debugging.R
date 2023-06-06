# Debugging

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
    if (!is.numeric(d)) {
        stop("`d` must be numeric", call. = FALSE)
    }
    d + 10
}


f('a')


#  22.3.1 Lazy evaluation 
# One drawback to traceback() is that it always linearises the call tree, which can be confusing if there is much lazy evaluation involved (Section 7.5.2). For example, take the following example where the error happens when evaluating the first argument to f():
j <- function() k()
k <- function() stop('Ooops!', call. = FALSE)
f(j())
traceback()

rlang::last_trace()


#  22.4 Interactive debugger 
# use browser()

g <- function(b) {
    browser()
    h(b)
}
f(10)
 
# browser() is just a regular function call which means that you can run it conditionally by wrapping it in an if statement:
g <- function(b) {
    if (b < 0) {
        browser()
    }
    h(b)
}

g(b = -1)
