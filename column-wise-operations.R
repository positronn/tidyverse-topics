# column-wise-operations.R
#
# It’s often useful to perform the same operation on multiple columns, but copying and
# pasting is both tedious and error prone:
library(dplyr)

mtcars %>% 
    group_by(cyl, hp) %>% 
    summarise(disp = mean(disp), mpg = mean(mpg), wt = mean(wt))

mtcars %>% 
    group_by(cyl, hp) %>% 
    summarise(across(disp:carb, mean))

# Basic usage 
# across() has two primary arguments:
# - The first argument, .cols, selects the columns you want to operate on.
#       It uses tidy selection (like select()) so you can pick variables
#       by position, name, and type.
# - The second argument, .fns, is a function or list of functions to
#       apply to each column. This can also be a purrr style formula
#       (or list of formulas) like ~ .x / 2. (This argument
#       is optional, and you can omit it if you just want to get
#       the underlying data;
starwars %>% 
    summarise(across(where(is.character), n_distinct))


starwars %>% 
    group_by(species) %>% 
    filter(n() > 1) %>% 
    summarise(across(c(sex, gender, homeworld), n_distinct))


starwars %>% 
    group_by(homeworld) %>% 
    filter(n() > 1) %>% 
    summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)))

# Because across() is usually used in combination with summarise()
# and mutate(), it doesn’t select grouping variables in order
# to avoid accidentally modifying them:
df <- tibble(g = c(1, 2, 3),
             x = c(-1, 1, 3),
             y = c(-1, -4, -9))
df %>% 
    group_by(g) %>% 
    summarise(across(where(is.numeric), sum))


# Multiple functions 
# You can transform each variable with more than one function
# by supplying a named list of functions or lambda
# functions in the second argument:
min_max <- list(
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
)

starwars %>% 
    summarise(across(where(is.numeric), min_max)) 


starwars %>% 
    summarise(across(where(is.numeric), min_max)) %>% 
    tidyr::gather(key = metric, value = value)

# Control how the names are created with the .names argument which takes a glue spec:
starwars %>% 
    summarise(across(where(is.numeric), min_max, .names = '{.fn}.{.col}'))

starwars %>%
    summarise(across(c(height, mass, birth_year), min_max, .names = "{.fn}_{.col}"))

# If you’d prefer all summaries with the same function to 
# be grouped together, you’ll have to expand the calls yourself:
starwars %>% 
    summarise(
        across(c(height, mass, birth_year), ~ min(., na.rm = TRUE), .names = "min_{.col}"),
        across(c(height, mass, birth_year), ~ max(., na.rm = TRUE), .names = "max_{.col}")
    )
    

# We cannot however use where(is.numeric) in that last case because the second across()
# would pick up the variables that were newly created (“min_height”,
# “min_mass” and “min_birth_year”).
# We can work around this by combining both calls to across() into a single
# expression that returns a tibble:
starwars %>% summarise(
    tibble(
        across(where(is.numeric), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
        across(where(is.numeric), ~max(.x, na.rm = TRUE), .names = "max_{.col}")  
    )
)

# Alternatively we could reorganize results with relocate():
starwars %>% 
    summarise(across(where(is.numeric), min_max, .names = '{.fn}.{.col}')) %>% 
    relocate(starts_with('max'))


# Current column 
# If you need to, you can access the name of the “current” column inside by
# calling cur_column(). This can be useful if you want to perform some sort
# of context dependent transformation that’s already encoded in a vector:
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10000, z = 100)
df %>% 
    mutate(across(all_of(names(mult)), ~ .x * mult[[cur_column()]]))


# Gotchas 
# Be careful when combining numeric summaries with where(is.numeric):
df <- tibble(x = c(1, 2, 3),
             y = c(1, 4, 9))
df %>% 
    summarise(n = n(),
              across(where(is.numeric), sd))
# ere n becomes NA because n is numeric, so the across() computes
# its standard deviation, and the standard deviation of
# 3 (a constant) is NA. You probably want to compute n() last
# to avoid this problem:
df %>% 
    summarise(across(where(is.numeric), sd),
              n = n())

# Alternatively, you could explicitly exclude n from the columns
# to operate on:
df %>% 
    summarise(n = n(),
              across(where(is.numeric) & !n, sd))

# Another approach is to combine both the call to n() and across()
# in a single expression that returns a tibble:
df %>% 
    summarise(
        tibble(n = n(),
               across(where(is.numeric), sd))
    )


# Other verbs
# So far we’ve focused on the use of across() with summarise(),
# but it works with any other dplyr verb that uses data masking:
# Rescale all numeric variables to range 0-1:
recale_01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
}
df <- tibble(x = 1:4, y = rnorm(4))
df %>% 
    mutate(across(where(is.numeric), recale_01))

# For some verbs, like group_by(), count() and distinct(), you don’t
# need to supply a summary function, but it can be useful to
# use tidy-selection to dynamically select a set of columns. In
# those cases, we recommend using the complement to across(), pick(),
# which works like across() but doesn’t apply any functions and
# instead returns a data frame containing the selected columns.

# Find all distinct
starwars %>% 
    distinct(pick(contains('color')))

# Count all combinations of variables with a given pattern:
starwars %>% 
    count(pick(contains('color')), sort = TRUE)

# filter() 
# We cannot directly use across() in filter() because we need an
# extra step to combine the results. To that end, filter()
# has two special purpose companion functions:
# 
# `if_any()` keeps the rows where the predicate is true
# for at least one selected column:
starwars %>% 
    filter(if_any(everything(), ~ !is.na(.)))
    
# `if_all()` keeps the rows where the predicate is true for
# all selected columns.
starwars %>% 
    filter(if_all(everything(), ~ !is.na(.)))

