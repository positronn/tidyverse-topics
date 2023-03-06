# splice.r
library(rlang)
library(tidyr)
# The splice operator !!! implemented in dynamic dots
# injects a list of arguments into a function call.
# It belongs to the family of injection operators and
# provides the same functionality as do.call().

# The two main cases for splice injection are:
# - Turning a list of inputs into distinct arguments.
#       This is especially useful with functions
#       that take data in ..., such as base::rbind().
dfs <- list(mtcars, mtcars)
inject(rbind(!!!dfs))

# - Injecting defused expressions like symbolised column names.
#   For tidyverse APIs, this second case is no longer as useful since dplyr 1.0
#       and the across() operator.


# Where does !!! work?
# !!! does not work everywhere, you can only use it within certain special functions:
# - Functions taking dynamic dots like list2().
# - Functions taking defused and data-masked arguments, which are dynamic by default.
# - Inside inject().
# Most tidyverse functions support !!! out of the box. With base
# functions you need to use inject() to enable !!!.



# Splicing a list of arguments
# Take a function like base::rbind() that takes data in .... This
# sort of functions takes a variable number of arguments.
df1 <- data.frame(x = 1)
df2 <- data.frame(x = 2)

rbind(df1, df2)


# Passing individual arguments is only possible for a fixed amount
# of arguments. When the arguments are in a list whose length is variable
# (and potentially very large), we need a programmatic approach like
# the splicing syntax !!!:
dfs <- list(df1, df2, data.frame(x = 3))
inject(
    rbind(!!!dfs)
)
rbind(!!!dfs)     # fails, needs inject function

# Because rbind() is a base function we used inject() to explicitly enable
#m!!!. However, many functions implement dynamic dots with !!! implicitly
#menabled out of the box.
tidyr::expand_grid(x = 1:2, y = c('a', 'b'))

xs <- list(x = 1:2, y = c('a', 'b'))
tidyr::expand_grid(!!!xs)
tidyr::expand_grid(xs)

# Note how the expanded grid has the right column names. That's because
# we spliced a named list. Splicing causes each name of the list to become
#an argument name.
set_names(xs, toupper)
tidyr::expand_grid(!!!set_names(xs, toupper))


my_mean <- function(.data, ...) {
    .data %>% dplyr::summarise(across(c(...), ~ mean(.x, na.rm = TRUE)))
}


# Performance of injected dots and dynamic dots
# Take this dynamic dots function:
n_args <- function(...) {
    length(list2(...))
}

# Because it takes dynamic dots you can splice with !!! out of the box.
n_args(1, 2)
n_args(!!!mtcars)

# Equivalently you could enable !!! explicitly with inject().
inject(n_args(!!!mtcars))

# While the result is the same, what is going on under the hood is
# completely different. list2() is a dots collector that special-cases !!!
# arguments. On the other hand, inject() operates on the language and creates
# a function call containing as many arguments as there are elements in the
# spliced list. If you supply a list of size 1e6, inject() is creating one million
# arguments before evaluation. This can be much slower
xs <- rep(list(1), 1e6)

system.time(
    n_args(!!!xs)
)

system.time(
    inject(
        n_args(!!!xs)
    )
)

# The same issue occurs when functions taking dynamic dots are called
# inside a data-masking function like dplyr::mutate(). The mechanism
# that enables !!! injection in these arguments is the same as in inject().
