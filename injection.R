# injection.r
# Injecting with !!, !!!, and glue syntax

# The injection operators are extensions of R implemented by rlang to
# modify a piece of code before R processes it. There are two main families:
# - the dynamic dots operators, !!!  and "{"
# - the metaprogramming operators !!, {{, and "{{". 
#        Splicing with !!! can also be done in metraprogramming context


# Dots injection
# Unlike regular ..., dynamic dots are programmable with injection operators.

# Splicing with !!!

# For instance, take a function like rbind() which takes
# data in .... To bind rows, you supply them as separate arguments:
rbind(a = 1:5, b = 6:10)

# But how do you bind a variable number of rows stored in a list?
# The base R solution is to invoke rbind() with do.call():
rows <- list(a = 1:5, b = 6:10)
do.call('rbind', rows)


# Functions that implement dynamic dots include a built-in
# way of folding a list of arguments in .... To illustrate this,
# we'll create a variant of rbind() that takes dynamic dots by collecting ... with list2():
rbind2 <- function(...) {
    do.call('rbind', list2(...))
}

rbind2(a = 1:5, b = 6:10)

# And a list of arguments can be supplied by splicing the list with !!!:
# paraphrasing: !!! injects a list of arguments
rbind2(!!!rows)
rbind2(!!!rows, c = 11:15)
rbind(!!!rows)      # original implementation fails

# A related problem comes up when an argument name is stored in a variable.
# With dynamic dots, you can inject the name using glue syntax with "{":
name <- 'foo'
rbind2('{name}' := 1:2, bar = 3:4)
rbind2('prefix_{name}' := 1:2, bar = 3:4)


# Metaprogramming injection

# Data-masked arguments support the following injection operators.
# They can also be explicitly enabled with inject().

# Embracing with {{
# The embracing operator {{ is made specially for function arguments.
# It defuses the expression supplied as argument and immediately injects
# it in place. The injected argument is then evaluated in another context
# such as a data mask.
# !! The embracing operator {{ is made specially for function arguments !!

# inject function arguments that might contain
# data-variables by embracing them with {{ }}
mean_by <- function(data, by, var) {
    data %>% 
        group_by({{ by }}) %>% 
        summarise(avg = mean({{ var }}, na.rm = TRUE))
}

# the data-variables `cyl` and `disp` inside the
# env-variables `by` and `var` are injected inside `group_by()`
# and `summarise()`
mtcars %>% 
    mean_by(by = cyl, var = disp)

# Injecting with !!
# Unlike !!! which injects a list of arguments, the injection operator
# !! (pronounced "bang-bang") injects a single object.
#  One use case for !! is to substitute an environment-variable
# (created with <-) with a data-variable (inside a data frame).

# the env-variable `var` contians a data-symbol object, in this
# case a reference to the data-variable `height`
var <- data_sym('disp')

# we inject the data-varaible contained in `var` inside `summarise()`
mtcars %>% 
    summarise(avg = mean(!!var, na.rm = TRUE))


# Another use case is to inject a variable by value to avoid name collisions.
df <- data.frame(x = 2)

# this name conflicsts with a column in `df`
x <- 100

# inject the env-variable
df %>% 
    mutate(x = x / !!x)

df %>% 
    mutate(x = x / x)

# Note that in most cases you don't need injection with !!.
# For instance, the .data and .env pronouns provide more intuitive
# alternatives to injecting a column name and injecting a value.

# Splicing with !!!
# The splice operator !!! of dynamic dots can also be used in metaprogramming
# context (inside data-masked arguments and inside inject()). For instance,
# we could reimplement the rbind2() function presented above using inject()
# instead of do.call():
rbind2 <- function(...) {
    rbind(!!!list(...)) %>% 
        inject()
}
# There are two things going on here. We collect ... with list2() so
# that the callers of rbind2() may use !!!. And we use inject() so
# that rbind2() itself may use !!! to splice the list of arguments
# passed to rbind2().


# summary
# !!! injects a list of arguments (must be used on function calls)
# !! injects single objects
# glue: "{" injects variables into strings
# glue: {{ }} injects and defuses objects (?variables?) inside functions
