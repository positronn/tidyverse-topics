# 11. Function Operators
# In this chapter, you’ll learn about function operators.
# A function operator is a function that takes one (or more)
# functions as input and returns a function as output. The
# following code shows a simple function operator, chatty().
# It wraps a function, making a new function that prints out its
# first argument. You might create a function like this because
# it gives you a window to see how functionals, like map_int(), work.

chatty <- function(f) {
    force(f)
    function(x, ...) {
        res <- f(x, ...)
        cat('Processing ', x, '\n', sep = '')
        res
    }
}

f <- function(x) x ^ 2
s <- c(1:5)

purrr::map_dbl(s, chatty(f))

# Function operators are closely related to function factories;
# indeed they’re just a function factory that takes a function
# as input.
# Like factories, there’s nothing you can’t do without them, but
# they often allow you to factor out complexity in order to
# make your code more readable and reusable.

# Function operators are typically paired with functionals.
# If you’re using a for-loop, there’s rarely a reason to use a
# function operator, as it will make your code more complex
# for little gain.

# If you’re familiar with Python, decorators is just another
# name for function operators.
library(purrr)
library(memoise)


# 11.2.1 Capturing errors with purrr::safely() 
# One advantage of for-loops is that if one of the iterations fails, you can still access all the results up to the failure:
x <- list(
    c(0.512, 0.165, 0.717),
    c(0.064, 0.781, 0.427),
    c(0.890, 0.785, 0.495),
    "oops"
)

out <- rep(NA_real_, length(x))
for (i in seq_along(x)) {
    out[[i]] <- sum(x[[i]])
}
out


# If you do the same thing with a functional, you get no output, making it hard to figure out where the problem lies:
map_dbl(x, sum)

# purrr::safely() provides a tool to help with this problem.
# safely() is a function operator that transforms a function
# to turn errors into data.
safe_sum <- safely(sum)
safe_sum

# Like all function operators, safely() takes a function and
# returns a wrapped function which we can call as usual:
str(safe_sum(x[[1]]))
str(safe_sum(x[[2]]))
str(safe_sum(x[[4]]))

# You can see that a function transformed by safely() always
# returns a list with two elements, result and error. If the
# function runs successfully, error is NULL and result contains
# the result; if the function fails, result is NULL and error contains the error.out 
out <- map(x, safely(sum))
str(out)


# The output is in a slightly inconvenient form, since we have four lists,
# each of which is a list containing the result and the error. We can make
# the output easier to use by turning it “inside-out” with purrr::transpose(),
# so that we get a list of results and a list of errors:
out <- map(x, safely(sum)) %>% 
    transpose()
str(out)


# Now we can easily find the results that worked, or the inputs that failed:
ok <- out$error %>% 
    map_lgl(is.null) 
ok
x[!ok]

out$result[ok]

# You can use this same technique in many different situations. For example,
# imagine you’re fitting a generalised linear model (GLM) to a list of
# data frames. GLMs can sometimes fail because of optimisation problems,
# but you still want to be able to try to fit all the models, and later
# look back at those that failed:
fit_model <- function(df) {
    glm(y ~ x1 + x2 * x3, data = df)
}

datasets <- list(mtcars = mtcars)

models <- datasets %>% 
    map(safely(fit_model))

ok <- models$error %>% 
    map_lgl(is.null)

# which failed to converge
datasets[!ok]

# whici models were successful
models[ok]


#  11.2.2 Caching computations with memoise::memoise() 
# Another handy function operator is memoise::memoise(). It memoises a function,
# meaning that the function will remember previous inputs and return cached
# results. Memoisation is an example of the classic computer science
# tradeoff of memory versus speed. A memoised function can run much faster,
# but because it stores all of the previous inputs and outputs, it uses more memory.

# Let’s explore this idea with a toy function that simulates an expensive operation:
slow_function <- function(x) {
    Sys.sleep(1)
    x * 10 * runif(1)
}
system.time(print(slow_function(1)))
system.time(print(slow_function(1)))

# When we memoise this function, it’s slow when we call it with new arguments.
# But when we call it with arguments that it’s seen before it’s instantaneous:
# it retrieves the previous value of the computation.
fast_function <- memoise::memoise(slow_function)
system.time(print(fast_function(1)))
system.time(print(fast_function(1)))

# A relatively realistic use of memoisation is computing the Fibonacci series.
#  A naive version is slow because, for example
fib <- function(n) {
    if (n < 2) return(1)
    fib(n - 2) + fib(n - 1)
}

system.time(fib(23))
system.time(fib(24))
system.time(lg_fib <- fib(35))
lg_fib

# Memoising fib() makes the implementation much faster because
# each value is computed only once:
fib2 <- memoise::memoise(
    function(n) {
        if (n < 2)
            return(1)
        fib2(n - 2) + fib2(n - 1)
    }
)

system.time(fib2(23))
system.time(fib2(24))
system.time(lg_fib <- fib2(35))
lg_fib

# This is an example of dynamic programming, where a complex problem
# can be broken down into many overlapping subproblems, and remembering
# the results of a subproblem considerably improves performance.


#  11.3 Case study: Creating your own function operators 
#
delay_by <- function(f, amount) {
    force(f)
    force(amount)
    
    function(...) {
        Sys.sleep(amount)
        f(...)
    }
}
system.time(runif(100))
system.time(delay_by(runif, 0.2)(100))

# Creating a function to display the occasional dot is a little harder,
# because we can no longer rely on the index from the loop. We could pass
# the index along as another argument, but that breaks encapsulation: a
# concern of the progress function now becomes a problem that the higher
# level wrapper needs to handle. Instead, we’ll use another function
# factory trick (from Section 10.2.4), so that the progress wrapper can
# manage its own internal counter:
dot_every <- function(f, n) {
    force(f)
    force(n)
    
    i <- 0
    function(...) {
        i <<- i + 1
        if (i %% n == 0)
            cat(".")
        f(...)
    }
}
walk(1:100, runif)
walk(1:100, dot_every(runif, 10))

