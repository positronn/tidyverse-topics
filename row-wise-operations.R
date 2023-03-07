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

