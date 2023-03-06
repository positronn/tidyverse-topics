# data-mask.r
# Data-masking is a distinctive feature of R whereby
# programming is performed directly on a data set,
# with columns defined as normal objects.

# unmasked programming
mean(mtcars$cyl + mtcars$am)

# referring to columns is an error - where is the data?
mean(cyl + am)

# data-masking
with(mtcars, mean(cyl + am))

# While data-masking makes it easy to program interactively with data frames,
# it makes it harder to create functions. Passing data-masked arguments to
# functions requires injection with the embracing operator {{ or, in more
# complex cases, the injection operator !!.

# Why does data-masking require embracing and injection?
# Injection (also known as quasiquotation) is a metaprogramming feature
# that allows you to modify parts of a program. This is needed because
# under the hood data-masking works by defusing R code to prevent its immediate
# evaluation. The defused code is resumed later on in a context where data frame
# columns are defined.

# Let's see what happens when we pass arguments to a data-masking
# function like summarise() in the normal way:
my_mean <- function(data, var1, var2) {
    dplyr::summarise(data, mean(var1 + var2))
}
my_mean(mtcars, cyl, am)

# The problem here is that summarise() defuses the R code it was
# supplied, i.e. mean(var1 + var2). Instead we want it to see
# mean(cyl + am). This is why we need injection, we need to modify
# that piece of code by injecting the code supplied to the function
# in place of var1 and var2.
# To inject a function argument in data-masked context, just embrace it with {{:
my_mean <- function(data, var1, var2) {
    dplyr::summarise(data, mean({{ var1 }} + {{ var2 }}))
}
my_mean(mtcars, cyl, am)


# What does "masking" mean?
# In normal R programming objects are defined in the current environment,
# for instance in the global environment or the environment of a function.
factor <- 1000

# Can now use `factor` in computations
mean(mtcars$cyl * factor)

# This environment also contains all functions currently in scope.
# In a script this includes the functions attached with library() calls;
# in a package, the functions imported from other packages. If evaluation was
# performed only in the data frame, we'd lose track of these objects and
# functions necessary to perform computations.

# To keep these objects and functions in scope, the data frame is
# inserted at the bottom of the current chain of environments. It
# comes first and has precedence over the user environment.
# In other words, it masks the user environment.

# Since masking blends the data and the user environment
# by giving priority to the former, R can sometimes use a data
# frame column when you really intended to use a local object.

# Defining an env-variable
cyl <- 1000


# Referring to a data-variable
dplyr::summarise(mtcars, mean(cyl))

cyl <- 1000
mtcars %>%
    dplyr::summarise(
        mean_data = mean(.data$cyl),
        mean_env = mean(.env$cyl)
    )
