library(magrittr)
library(rlang)
library(dplyr)

# return the result of `1 + 1`
1 + 1

# return the expression `1 + 1`
expr(1 + 1)

# Evaluation of a defused expression can be resumed at any
# time with eval() (see also eval_tidy()).
e <- expr(1 + 1)
e %>% eval

# The most common use case for defusing expressions is
# to resume its evaluation in a data mask.
# This makes it possible for the expression to refer to
# columns of a data frame as if they were regular objects.
mean(cyl)               # error
e <- expr(mean(cyl))    # works
eval(e, mtcars)         # worls


mtcars %>% 
    summarise(mean(cyl))    # this is defused and data-masked !

# It is important to know that a function defuses its arguments
# because it requires slightly different methods when called from a function.
# The main thing is that arguments must be transported with the
# embrace operator {{. It allows the data-masking function to defuse
# the correct expression.
my_mean <- function(data, var) {
    dplyr::summarise(data, mean = mean({{ var }}))
}

mtcars %>% 
    my_mean(cyl)

mtcars %>% 
    my_mean(disp)

mtcars %>% 
    my_mean(hp)



# The term "defusing" comes from an analogy to the evaluation model
# in R. As you may know, R uses lazy evaluation, which means that
# arguments are only evaluated when they are needed for a computation.
# Let's take two functions, ignore() which doesn't do anything with its
# argument, and force() which returns it:
ignore <- function(arg) { NULL }
force <- function(arg) { arg }

ignore(warning('boom'))
force(warning('boom'))
# A warning is only emitted when the function actually triggers
# evaluation of its argument.

# Evaluation of arguments can be chained by passing them to other
# functions. If one of the functions ignores its argument,
# it breaks the chain of evaluation.
f <- function(x) { g(x) }
g <- function(y) { h(y) }
h <- function(z) { ignore(z) }

f(warning('boom'))

# In a way, arguments are like booby traps which explode (evaluate)
# when touched. Defusing an argument can be seen as defusing the booby trap.
expr(force(warning('boom')))


# Types of defused expressions
# - Calls, like f(1, 2, 3) or 1 + 1 represent the action of calling
#       a function to compute a new value, such as a vector.
# - Symbols, like x or df, represent named objects. When the object
#       pointed to by the symbol was defined in a function or
#       in the global environment, we call it an environment-variable.
#       When the object is a column in a data frame, we call it a data-variable.
# - Constants, like 1 or NULL
# You can create new call or symbol objects by using the defusing function expr():

# create a symbol representing objects called `foo`
expr(foo)

# create a call representing the computaiton of the mean
# of `foo`
expr(mean(foo, na.rm = TRUE))

# return a constant
expr(1)

expr(NULL)

# Defusing is not the only way to create defused expressions. You can also assemble them from data:
# assembe a symbol from a string
var <- 'foo'
sym(var)

# assemble a call from strings, symbols and constants
call('mean', sym(var), na.rm = TRUE)


# edfuse and inject
# One purpose for defusing evaluation of an expression is to interface with
# data-masking functions by injecting the expression back into another
# function with !!. This is the defuse-and-inject pattern.
my_summarise <- function(data, arg) {
    # defuse the user expression in `arg`
    arg <- enquo(arg)
    # inject the expression contained in `arg`
    # inside a `summarise()` agument
    data %>% 
        summarise(mean = mean(!!arg, na.rm = TRUE))
}

# Defuse-and-inject is usually performed in a single step with the embrace operator {{.
my_summarise_two <- function(data, arg) {
    data %>% 
        summarise(mean = mean({{ arg }}, na.rm = TRUE))
}


# Using enquo() and !! separately is useful in more complex cases where you need
# access to the defused expression instead of just passing it on.


# Defused arguments and quosures
# If you inspect the return values of expr() and enquo(), you'll notice
# that the latter doesn't return a raw expression like the former.
# Instead it returns a quosure, a wrapper containing an expression and
# an environment.
expr(1 + 1)

my_function <- function(arg) enquo(arg)
my_function(1 + 1)

# R needs information about the environment to properly evaluate
# argument expressions because they come from a different context
# than the current function. For instance when a function in your
# package calls dplyr::mutate(), the quosure environment indicates
# where all the private functions of your package are defined.




