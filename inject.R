# inject.r


# `inject()` evaluates and expression with injection support.
# there are three main usages:
# - splicing lists of arguments in a function call
# - Inline objects or other expressions in an expression with
#       !! and !!!. For instance to create functions
#       or formulas programmatically.
# - Pass arguments to NSE functions that defuse their arguments
#       without injection support (see for instance enquo0()).
#       You can use {{ arg }} with functions documented to support
#       quosures. Otherwise, use !!enexpr(arg).
#
# inject(expr, env = caller_env())

# inject simply evaluates its argument with injection support.
# these expressions are equivalent
2 * 3
inject(2 * 3)
inject(!!2 * !!3)

# injection with `!!` can be useful to insert objects or expressions
# within other expressions, like formulas:
lhs <- sym('foo')
rhs <- sym('bar')
inject(!!lhs ~ !!rhs + 10)
expr(!!lhs ~ !!rhs + 10)


# injection with `!!!` splices lists of arguments in function calls:
args <- list(na.rm = TRUE, finite = 0.2)
inject(mean(1:10, !!!args))
e <- expr(mean(1:10, !!!args))
e %>% eval



# Injectin Values

# Data-masking functions like with() are handy
# because you can refer to column names in your computations.
# This comes at the price of data mask ambiguity: if you have defined an
# env-variable of the same name as a data-variable, you get a name
# collisions. This collision is always resolved by giving precedence
# to the data-variable (it masks the env-variable):
cyl <- c(100, 110)
with(mtcars, mean(cyl))    

# The injection operator offers one way of solving this.
# Use it to inject the env-variable inside the data-masked expression:    
inject(
    with(mtcars, mean(!!cyl))
)
# Note that the .env pronoun is a simpler way of solving the ambiguity.
# See The data mask ambiguity for more about this.

# Injecting expressions
# Injection is also useful for modifying parts of a defused expression.
# In the following example we use the symbolise-and-inject
# pattern to inject a column name inside a data-masked expression.
var <- sym('cyl')
inject(
    with(mtcars, mean(!!var))
)
# Since with() is a base function, you can't inject quosures, only
# naked symbols and calls. This isn't a problem here because we're
# injecting the name of a data frame column. If the environment is important,
# try injecting a pre-computed value instead.


# When do I need !!?
# With tidyverse APIs, injecting expressions with !! is no longer
# a common pattern. First, the .env pronoun solves the ambiguity
# problem in a more intuitive way:
cyl <- 100
mtcars %>% 
    dplyr::mutate(cyl_ = cyl * .env$cyl)

# Second, the embrace operator {{ makes the defuse-and-inject
# pattern easier to learn and use.
my_mean <- function(data, var) {
    data %>% 
        dplyr::summarise(mean({{ var }}))
}

# equivalent to
my_mean_2 <- function(data, var) {
    data %>% 
        dplyr::summarise(mean(!!enquo(var)))
}
