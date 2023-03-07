# iteration.R
#  In this chapter you’ll learn about two important iteration paradigms: imperative programming
# and functional programming. On the imperative side you have tools like for loops and while loops,
# which are a great place to start because they make iteration very explicit, so it’s
# obvious what’s happening. However, for loops are quite verbose, and require quite
# a bit of bookkeeping code that is duplicated for every for loop.
# Functional programming (FP) offers tools to extract out this duplicated code,
# so each common for loop pattern gets its own function. Once you master the vocabulary of FP,
# you can solve many common iteration problems with less code, more ease, and fewer errors.
library(purrr)


# Imagine we have this simple tibble:
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)
df

# We want to compute the median of each column. You could do with copy-and-paste:
median(df$a)
median(df$b)
median(df$c)
median(df$d)

# But that breaks our rule of thumb: never copy and paste more than twice.
# Instead, we could use a for loop:
output <- vector('double', ncol(df))    # output
for (i in seq_along(df)) {              # sequence
    output[[i]] <- median(df[[i]])      # body
}
output

# Every for loop has three components:
# 1. The output: output <- vector("double", length(x)). Before you start the loop,
    # you must always allocate sufficient space for the output. This is very
    # important for efficiency: if you grow the for loop at each iteration using
    # c() (for example), your for loop will be very slow.
    # A general way of creating an empty vector of given length is the vector() function.
    # It has two arguments: the type of the vector (“logical”, “integer”, “double”,
    # “character”, etc) and the length of the vector.
# 2. The sequence: i in seq_along(df). This determines what to loop over: each run of the
#   for loop will assign i to a different value from seq_along(df). It’s useful to think
#   of i as a pronoun, like “it”.
#   You might not have seen seq_along() before. It’s a safe version of
#   the familiar 1:length(l), with an important difference: if you have a
#   zero-length vector, seq_along() does the right thing:
y <- vector('double', 0)
seq_along(y)
1:length(y)
# 3. The body: output[[i]] <- median(df[[i]]). This is the code that does the work.
#   It’s run repeatedly, each time with a different value for i. The first
#   iteration will run output[[1]] <- median(df[[1]]), the second will
#   run output[[2]] <- median(df[[2]]), and so on.


#  21.3 For loop variations 
# Once you have the basic for loop under your belt, there are some variations that
# you should be aware of. These variations are important regardless of how you do
# iteration, so don’t forget about them once you’ve mastered the FP techniques
# you’ll learn about in the next section.

# There are four variations on the basic theme of the for loop:
    
# Modifying an existing object, instead of creating a new object.
# Looping over names or values, instead of indices.
# Handling outputs of unknown length.
# Handling sequences of unknown length.


#  21.4 For loops vs. functionals 
# For loops are not as important in R as they are in other languages because R is a
# functional programming language. This means that it’s possible to wrap up for
# loops in a function, and call that function instead of using the for loop directly.
df <- tibble(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

# Imagine you want to compute the mean of every column. You could do that with a for loop:
output <- vector("double", length(df))
for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
}
output

# You realise that you’re going to want to compute the means of every column pretty
# frequently, so you extract it out into a function:
col_mean <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
        output[i] <- mean(df[[i]])
    }
    output
}
# But then you think it’d also be helpful to be able to compute the median,
# and the standard deviation, so you copy and paste your col_mean() function and
# replace the mean() with median() and sd()
col_median <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
        output[i] <- median(df[[i]])
    }
    output
}
col_sd <- function(df) {
    output <- vector("double", length(df))
    for (i in seq_along(df)) {
        output[i] <- sd(df[[i]])
    }
    output
}

# Uh oh! You’ve copied-and-pasted this code twice, so it’s time to think about
# how to generalise it. Notice that most of this code is for-loop boilerplate and
# it’s hard to see the one thing (mean(), median(), sd()) that is different between the functions.

col_summary <- function(df, fun) {
    out <- vector("double", length(df))
    for (i in seq_along(df)) {
        out[i] <- fun(df[[i]])
    }
    out
}

# The idea of passing a function to another function is an extremely powerful idea,
# and it’s one of the behaviours that makes R a functional programming language.
# It might take you a while to wrap your head around the idea, but it’s worth the
# investment. In the rest of the chapter, you’ll learn about and use the purrr package,
# which provides functions that eliminate the need for many common for loops. The apply
# family of functions in base R (apply(), lapply(), tapply(), etc) solve a similar problem,
# but purrr is more consistent and thus is easier to learn.

# The goal of using purrr functions instead of for loops is to allow you to
# break common list manipulation challenges into independent pieces:
# How can you solve the problem for a single element of the list? Once you’ve
# 1. solved that problem, purrr takes care of generalising your solution to every element in the list.
# 2. If you’re solving a complex problem, how can you break it down into
#   bite-sized pieces that allow you to advance one small step towards a
#   solution? With purrr, you get lots of small pieces that you can
#   compose together with the pipe.

#  21.5 The map functions 
# The pattern of looping over a vector, doing something to each element and
# saving the results is so common that the purrr package provides a
# family of functions to do it for you. There is one function for each type of output:
#    map() makes a list.
#    map_lgl() makes a logical vector.
#    map_int() makes an integer vector.
#    map_dbl() makes a double vector.
#    map_chr() makes a character vector.

# Each function takes a vector as input, applies a function to each piece,
# and then returns a new vector that’s the same length (and has the same names)
# as the input. The type of the vector is determined by the suffix to the map function.


# We can use these functions to perform the same computations as the last for loop.
# Those summary functions returned doubles, so we need to use map_dbl():
df
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

map_dbl(df, mean) %>% 
    typeof()

# Compared to using a for loop, focus is on the operation being performed
# (i.e. mean(), median(), sd()), not the bookkeeping required to loop
# over every element and store the output. This is even more apparent
# if we use the pipe:
df %>% 
    map_dbl(mean)

# There are a few differences between map_*() and col_summary():
# All purrr functions are implemented in C. This makes them a little
#   faster at the expense of readability.
# The second argument, .f, the function to apply, can be a formula, a
#   character vector, or an integer vector. You’ll learn about
#   those handy shortcuts in the next section.
# map_*() uses … ([dot dot dot]) to pass along additional
#   arguments to .f each time it’s called:
df %>% 
    map_dbl(mean, trim = 0.5)
# The map functions also preserve names:
(z <- list(x = 1:3,
          y = 4:5))
z %>% 
    map_int(length)


#  21.5.1 Shortcuts 
# There are a few shortcuts that you can use with .f in order to save a little typing.
# Imagine you want to fit a linear model to each group in a dataset. The
# following toy example splits up the mtcars dataset into three pieces
# (one for each value of cylinder) and fits the same linear model to each piece:
models <- mtcars %>% 
    split(.$cyl) %>% 
    map(function(df) lm(mpg ~ wt, data = df))
models

# The syntax for creating an anonymous function in R is quite verbose
# so purrr provides a convenient shortcut: a one-sided formula.
models <- mtcars %>% 
    group_split(cyl) %>% 
    map(~ lm(mpg ~ wt, data = .))
models
# Here I’ve used . as a pronoun: it refers to the current list element
# (in the same way that i referred to the current index in the for loop).
# When you’re looking at many models, you might want to extract a summary
# statistic like the R2. To do that we need to first run summary() and
# then extract the component called r.squared. We could do that using
# the shorthand for anonymous functions:
models %>% 
    map(summary) %>% 
    map_dbl(~ .$r.squared)


# But extracting named components is a common operation,
# so purrr provides an even shorter shortcut: you can use a string.
models %>% 
    map(summary) %>% 
    map_dbl('r.squared')

# You can also use an integer to select elements by position:
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% 
    map_dbl(2)


# 21.6 Dealing with failure 