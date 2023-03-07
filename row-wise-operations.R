# row-wise-operations.R
library(dplyr)

# dplyr, and R in general, are particularly well suited to performing operations
# over columns, and performing operations over rows is much harder. In this vignette,
# you’ll learn dplyr’s approach centred around the row-wise data frame created by rowwise().

# There are three common use cases that we discuss in this vignette:
# 
# Row-wise aggregates (e.g. compute the mean of x, y, z).
# Calling a function multiple times with varying arguments.
# Working with list-columns.

# These types of problems are often easily solved with a for loop, but it’s nice to have a
# solution that fits naturally into a pipeline.

df <- tibble(x = 1:2, y = 3:4, z = 5:6)

df %>% 
    mutate(m = mean(c(x, y, z)))


df %>% 
    rowwise() %>% 
    mutate(m = mean(c(x, y, z)))

df %>% 
    mutate(m = (x + y + z) / 3)

# If you use mutate() with a regular data frame, it computes the mean of x, y, and z
# across all rows. If you apply it to a row-wise data frame, it computes the mean for each row.

# You can optionally supply “identifier” variables in your call to rowwise().
# These variables are preserved when you call summarise(), so they behave somewhat
# similarly to the grouping variables passed to group_by():
df <- tibble(name = c("Mara", "Hadley"), x = 1:2, y = 3:4, z = 5:6)
df

df %>% 
    rowwise() %>% 
    summarise(m = mean(c(x, y, z)))


df %>% 
    rowwise(name) %>% 
    summarise(m = mean(c(x, y, z)))

# rowwise() is just a special form of grouping, so if you want to remove it
# from a data frame, just call ungroup().

# dplyr::summarise() makes it really easy to summarise values across rows within one column.
# When combined with rowwise() it also makes it easy to summarise values across columns
# within one row. 
df <- tibble(id = 1:6, w = 10:15, x = 20:25, y = 30:35, z = 40:45)
df
rf <- df %>% rowwise(id)

rf %>% 
    mutate(total = sum(c(w, x, y, z)))

rf %>%
    summarise(total = sum(c(w, x, y, z)))

# Of course, if you have a lot of variables, it’s going to be tedious to type in every
# variable name. Instead, you can use c_across() which uses tidy selection syntax so
# you can to succinctly select many variables:

rf %>%
    mutate(total = sum(c_across(w:z)))


rf %>%
    mutate(total = sum(c_across(where(is.numeric))))


# to compute the proportion of the total for each column:
df %>%
    rowwise() %>% 
    mutate(total = sum(c_across(w:z))) %>% 
    ungroup() %>% 
    mutate(across(w:z, ~ . / total))


# Row-wise summary functions 
# The rowwise() approach will work for any summary function. But if you need greater speed,
# it’s worth looking for a built-in row-wise variant of your summary function. These are more
# efficient because they operate on the data frame as whole; they don’t split it into rows,
# compute the summary, and then join the results back together again.
df %>%
    mutate(total = rowSums(pick(where(is.numeric))))

df %>%
    mutate(mean = rowMeans(pick(where(is.numeric))))
# NB: I use df (not rf) and pick() (not c_across()) here because rowMeans() and rowSums() take
# a multi-row data frame as input.


# List-columns 
# rowwise() operations are a natural pairing when you have list-columns. They allow you
# to avoid explicit loops and/or functions from the apply() or purrr::map() families.

# Imagine you have this data frame, and you want to count the lengths of each element:
df <- tibble(
    x = list(1, 2:3, 4:6)
)
# You might try calling length():
df %>% mutate(l = length(x))

# But that returns the length of the column, not the length of the individual values. 
# f you’re an R documentation aficionado, you might know there’s already a base R
# function just for this purpose:
df %>%
    mutate(l = lengths(x))

# Or if you’re an experienced R programmer, you might know how to apply a function
# to each element of a list using sapply(), vapply(), or one of the purrr map() functions:
df %>%
    mutate(l = sapply(x, length))

df %>%
    mutate(l = purrr::map_int(x, length))

# But wouldn’t it be nice if you could just write length(x) and dplyr would figure
# out that you wanted to compute the length of the element inside of x? Since you’re here,
# you might already be guessing at the answer: this is just another application
# of the row-wise pattern.
df %>% 
    rowwise() %>% 
    mutate(l = length(x))


# Subsetting

# Before we continue on, I wanted to briefly mention the magic that makes this
# work. This isn’t something you’ll generally need to think about (it’ll just work),
# but it’s useful to know about when something goes wrong.

# There’s an important difference between a grouped data frame where each
# group happens to have one row, and a row-wise data frame where every group
# always has one row. Take these two data frames:
df <- tibble(g = 1:2, y = list(1:3, "a"))

gf <- df %>%
    group_by(g)
rf <- df %>%
    rowwise(g)

# If we compute some properties of y, you’ll notice the results look different:
gf %>%
    mutate(type = typeof(y), length = length(y))

rf %>%
    mutate(type = typeof(y), length = length(y))

# They key difference is that when mutate() slices up the columns to pass to
# length(y) the grouped mutate uses [ and the row-wise mutate uses [[. The
# following code gives a flavour of the differences if you used a for loop:
# grouped
out1 <- integer(2)
for (i in 1:2) {
    out1[[i]] <- length(df$y[i])
}
out1

# rowwise
out2 <- integer(2)
for (i in 1:2) {
    out2[[i]] <- length(df$y[[i]])
}
out2



# Modelling 
# rowwise() data frames allow you to solve a variety of modelling problems
# in what I think is a particularly elegant way. We’ll start by creating a nested data frame:
by_cyl <- mtcars %>%
    nest_by(cyl)
by_cyl

# This is a little different to the usual group_by() output: we have visibly changed
# the structure of the data. Now we have three rows (one for each group), and we have a
# list-col, data, that stores the data for that group. Also note that the output is rowwise();
# this is important because it’s going to make working with that list of data frames much easier.
# Once we have one data frame per row, it’s straightforward to make one model per row:
mods <- by_cyl %>%
    mutate(mod = list(lm(mpg ~ wt, data = data)))
mods

# And supplement that with one set of predictions per row:
mods <- mods %>%
    mutate(pred = list(predict(mod, data)))
mods


# You could then summarise the model in a variety of ways:
mods %>%
    summarise(rmse = sqrt(mean((pred - data$mpg) ^ 2)))

mods %>%
    summarise(rsq = summary(mod)$r.squared)

mods %>% summarise(broom::glance(mod))


# Or easily access the parameters of each model:
mods %>% reframe(broom::tidy(mod))


# Repeated function calls 
# rowwise() doesn’t just work with functions that return a length-1 vector
# (aka summary functions); it can work with any function if the result is a list.
# This means that rowwise() and mutate() provide an elegant way to call a
# function many times with varying arguments, storing the outputs alongside the inputs.

# Simulations 
# I think this is a particularly elegant way to perform simulations, because
# it lets you store simulated values along with the parameters that generated them.
# For example, imagine you have the following data frame that describes the properties
# of 3 samples from the uniform distribution:
df <- tribble(
    ~ n, ~ min, ~ max,
    1,     0,     1,
    2,    10,   100,
    3,   100,  1000,
)

df %>% 
    rowwise() %>% 
    mutate(data = list(runif(n, min, max))) 

# unpackung values
df %>% 
    rowwise() %>% 
    mutate(data = list(runif(n, min, max))) %>% 
    pull(data) 


# Note the use of list() here - runif() returns multiple values and a mutate()
# expression has to return something of length 1. list() means that we’ll get a
# list column where each row is a list containing multiple values. If you forget
# to use list(), dplyr will give you a hint:
df %>% 
    rowwise() %>% 
    mutate(data = runif(n, min, max))



# Multiple combinations 
# What if you want to call a function for every combination of inputs?
# You can use expand.grid() (or tidyr::expand_grid()) to generate the data
# frame and then repeat the same pattern as above:
df <- expand.grid(mean = c(-1, 0, 1), sd = c(1, 10, 100))
df %>% 
    rowwise() %>% 
    mutate(data = list(rnorm(10, mean, sd)))


# Varying functions 
# In more complicated problems, you might also want to vary the function being called.
# This tends to be a bit more of an awkward fit with this approach because the columns
# in the input tibble will be less regular. But it’s still possible, and it’s a natural
# place to use do.call():
df <- tribble(
    ~rng,     ~params,
    "runif",  list(n = 10), 
    "rnorm",  list(n = 20),
    "rpois",  list(n = 10, lambda = 5),
) %>%
    rowwise()

df %>% 
    mutate(data = list(do.call(rng, params)))
