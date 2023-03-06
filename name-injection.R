# name-injection.r
library(rlang)
library(magrittr)
# Dynamic dots (and data-masked dots which are dynamic by default) have built-in
# support for names interpolation with the glue package.

tibble::tibble(foo = 1)

foo <- 'name'
tibble::tibble('{foo}' = 1)
tibble::tibble(foo := 1)
tibble::tibble('{foo}' := 1)   # must use := with glue syntax


# Inside functions, embracing an argument with {{ inserts the
# expression supplied as argument in the string. This gives an
# indication on the variable or computation supplied as argument:
tib <- function(x) {
    tibble::tibble('var: {{ x }}' := x)
}
tib(1 + 1)


# See also englue() to string-embrace outside of dynamic dots.
g <- function(x) {
    englue('var: {{ x }}')
}
g(1 + 1)

# Technically, "{{" defuses a function argument, calls as_label()
# on the expression supplied as argument, and inserts the result in the string.

my_mean <- function(data, var) {
    data %>% dplyr::summarise("{{ var }}" := mean({{ var }}))
}

mtcars %>%
    my_mean(cyl)

mtcars %>% 
    my_mean(cyl * am)

# "{{" is only meant for inserting an expression supplied as argument
# to a function. The result of the expression is not inspected or used.
# To interpolate a string stored in a variable, use the regular glue operator
# "{" instead:
my_mean <- function(data, var, name = 'mean') {
    data %>% 
        dplyr::summarise('{name}' := mean({{ var }}))
}
mtcars %>% 
    my_mean(cyl)

mtcars %>% 
    my_mean(cyl, name = 'cyl')


# Using the wrong operator causes unexpected results:
x <- 'name'
list2('{{ x }}' := 1)
list2('{x}' := 1)


# Allow overriding default names
# The implementation of my_mean() in the previous section forces a default
# name onto the result. But what if the caller wants to give it a different
# name? In functions that take dots, it is possible to just supply a named
# expression to override the default. In a function like my_mean() that takes a
# named argument we need a different approach.
# This is where englue() becomes useful. We can pull out the default name
# creation in another user-facing argument like this:
my_mean <- function(data, var, name = englue('{{ var }}')) {
    data %>% 
        dplyr::summarise('{name}' := mean({{ var }}))
}

mtcars %>% 
    my_mean(cyl * am)

mtcars %>% 
    my_mean(cyl * am, name = 'mean_cyl_am')

