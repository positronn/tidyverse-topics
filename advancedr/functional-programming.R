# functional-programming.R
#
# 

library(purrr)

triple <- function(x) x * 3
1:3
map(1:3, triple)

# a simple (but fundamental) implementation of a map function
simple_map <- function(x, f, ...) {
    out <- vector('list', length(x))
    for (i in seq_along(x)) {
        out[[i]] <- f(x[[i]], ...)
    }
    return(out)
}

simple_map(1:3, triple)

# this is baser's equivalent to purrr's map
lapply(1:3, triple)

#  9.2.1 Producing atomic vectors 
# map() returns a list, which makes it the most general of the map family because
# you can put anything in a list. But it is inconvenient to return a list when
# a simpler data structure would do, so there are four more specific variants:
# map_lgl(), map_int(), map_dbl(), and map_chr(). Each returns an atomic vector
# of the specified type:
# map_chr() always returns a character vector
map_chr(mtcars, typeof)

# map_lgl() always returns a logical vector
map_lgl(mtcars, is.double)

# map_int() always returns a integer vector
n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)

# map_dbl() always returns a double vector
map_dbl(mtcars, mean)

map_int(1:3, triple)


# purrr uses the convention that suffixes, like _dbl(), refer to the output.
# All map_*() functions can take any type of vector as input.

# These examples rely on two facts: mtcars is a data frame, and data
# frames are lists containing vectors of the same length. This is more obvious
# if we draw a data frame with the same orientation as vector:

# All map functions always return an output vector the same length as the input,
# which implies that each call to .f must return a single value. If it does not,
# you’ll get an error:
pair <- function(x) c(x, x)
map_dbl(1:2, pair)
#> Error: Result 1 must be a single double, not an integer vector of length 2


# This is similar to the error you’ll get if .f returns the wrong type of result:
map_dbl(1:2, as.character)
#> Error: Can't coerce element 1 from a character to a double

# In either case, it’s often useful to switch back to map(), because map() can
# accept any type of output. That allows you to see the problematic output,
# and figure out what to do with it.
map(1:3, pair)
map(1:3, as.character)

# Base R has two apply functions that can return atomic vectors: sapply() and vapply().
# I recommend that you avoid sapply() because it tries to simplify the result, so
# it can return a list, a vector, or a matrix. 

# This makes it difficult to program with, and it should be avoided in
# non-interactive settings. vapply() is safer because it allows you to provide a template,
# FUN.VALUE, that describes the output shape. If you don’t want to use purrr,
# I recommend you always use vapply() in your functions, not sapply(). The primary
# downside of vapply() is its verbosity: for example, the equivalent to
# map_dbl(x, mean, na.rm = TRUE) is vapply(x, mean, na.rm = TRUE, FUN.VALUE = double(1)).

# 9.2.2 Anonymous functions and shortcuts 
# Instead of using map() with an existing function, you can create an inline
# anonymous function (as mentioned in Section 6.2.3):
map_dbl(mtcars, function(x) length(unique(x)))

# Anonymous functions are very useful, but the syntax is verbose. So purrr supports a special shortcut:
map_dbl(mtcars, ~ length(unique(.x)))


# This works because all purrr functions translate formulas,
# created by ~ (pronounced “twiddle”), into functions. You can see what’s
# happening behind the scenes by calling as_mapper():
as_mapper(~ length(unique(.x)))


# The function arguments look a little quirky but allow you to refer to . for
# one argument functions, .x and .y for two argument functions, and ..1, ..2, ..3, etc,
# for functions with an arbitrary number of arguments. . remains for backward
# compatibility but I don’t recommend using it because it’s easily confused
# with the . used by magrittr’s pipe.

# This shortcut is particularly useful for generating random data:
x <- map(1:3, ~ runif(2))
str(x)


# The map functions also have shortcuts for extracting elements from a vector,
# powered by purrr::pluck(). You can use a character vector to select elements
# by name, an integer vector to select by position, or a list to select by both
# name and position. These are very useful for working with deeply nested lists,
# which often arise when working with JSON.
x <- list(
    list(-1, x = 1, y = c(2), z = "a"),
    list(-2, x = 4, y = c(5, 6), z = "b"),
    list(-3, x = 8, y = c(9, 10, 11))
)
x

map_dbl(x, 'x')
map_dbl(x, 2)   # equivalent to the one below

map_dbl(x, 1)

map_dbl(x, list("y", 1))

map_chr(x, "z")
# Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)

# In base R functions, like lapply(), you can provide the name of the function as a string.
# This isn’t tremendously useful as lapply(x, "f") is almost always equivalent to
# lapply(x, f) and is more typing.


#  9.2.3 Passing arguments with ... 
# It’s often convenient to pass along additional arguments to the function
# that you’re calling. For example, you might want to pass na.rm = TRUE
# along to mean(). One way to do that is with an anonymous function:
x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))

# but because the map function pass ... along, there's a simpler form available
map_dbl(x, mean, na.rm = TRUE)

# any arguments that come after f in the call to map() are inserted after the data
# in individual calls to f().

# It’s important to note that these arguments are not decomposed; or said another way,
# map() is only vectorised over its first argument. If an argument after f is a
# vector, it will be passed along as is.

# Note there’s a subtle difference between placing extra arguments inside an anonymous
# function compared with passing them to map(). Putting them in an anonymous function
# means that they will be evaluated every time f() is executed, not just once when you
# call map(). This is easiest to see if we make the additional argument random:
plus <- function(x, y) x + y
x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
map_dbl(x, ~ plus(.x, runif(1)))


#  9.2.4 Argument names 
# In the diagrams, I’ve omitted argument names to focus on the overall structure.
# But I recommend writing out the full names in your code, as it makes it easier
# to read. map(x, mean, 0.1) is perfectly valid code, but will call mean(x[[1]], 0.1)
# so it relies on the reader remembering that the second argument to mean() is trim.
# To avoid unnecessary burden on the brain of the reader,
# be kind and write map(x, mean, trim = 0.1).
# 
# This is the reason why the arguments to map() are a little odd: instead of
# being x and f, they are .x and .f. It’s easiest to see the problem that leads
# to these names using simple_map() defined above. simple_map() has arguments x
# and f so you’ll have problems whenever the function you are calling has arguments x or f.
bootstrap_summary <- function(x, f) {
    f(sample(x, replace = TRUE))
}

simple_map(mtcars, bootstrap_summary, f = mean)
# Error in mean.default(x[[i]], ...): 'trim' must be numeric of length one

# The error is a little bewildering until you remember that the call to simple_map()
# is equivalent to simple_map(x = mtcars, f = mean, bootstrap_summary) because named
# matching beats positional matching.

# purrr functions reduce the likelihood of such a clash by using .f and .x
# instead of the more common f and x. Of course this technique isn’t perfect (because
# the function you are calling might still use .f and .x), but it avoids 99% of issues.
# The remaining 1% of the time, use an anonymous function.



#  9.2.5 Varying another argument 
# So far the first argument to map() has always become the first argument to the function.
# But what happens if the first argument should be constant, and you want to
# vary a different argument?

# It turns out that there’s no way to do it directly, but there are two tricks
# you can use instead. To illustrate them, imagine I have a vector that contains
# a few unusual values, and I want to explore the effect of different amounts of
# trimming when computing the mean. In this case, the first argument to mean()
# will be constant, and I want to vary the second argument, trim.
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

# The simplest technique is to use an anonymous function to rearrange the argument order:
map_dbl(trims, ~ mean(x, trim = .x))

# This is still a little confusing because I’m using both x and .x. You can make it
# a little clearer by abandoning the ~ helper:
map_dbl(trims, function(trim) mean(x, trim = trim))



#  9.3 Purrr style 
# Before we go on to explore more map variants, let’s take a quick look at how you
# tend to use multiple purrr functions to solve a moderately realistic problem:
# fitting a model to each subgroup and extracting a coefficient of the model.
# For this toy example, I’m going to break the mtcars data set down into groups
# defined by the number of cylinders, using the base split function:
by_cyl <- split(mtcars, mtcars$cyl)
by_cyl

# Now imagine we want to fit a linear model, then extract the second
# coefficient (i.e. the slope). The following code shows how you might do that with purrr:
by_cyl %>% 
    map(~ lm(mpg ~ wt, data = .x)) %>% 
    map(coef) %>% 
    map_dbl(2)


# How would you attack this problem with base R? You certainly could replace
# each purrr function with the equivalent base function:
by_cyl %>% 
    lapply(function(data) lm(mpg ~ wt, data = data)) %>% 
    lapply(coef) %>% 
    vapply(function(x) x[[2]], double(1))

# But this isn’t really base R since we’re using the pipe. To tackle purely in base
# I think you’d use an intermediate variable, and do more in each step:
models <- lapply(by_cyl, function(data) lm(mpg ~ wt, data = data))
vapply(models, function(x) coef(x)[[2]], double(1))

# Or, of course, you could use a for loop:
slopes <- double(length(by_cyl))
for (i in seq_along(by_cyl)) {
    model <- lm(mpg ~ wt, data = by_cyl[[i]])
    slopes[[i]] <- coef(model)[[2]]
}
slopes


# It’s interesting to note that as you move from purrr to base apply
# functions to for loops you tend to do more and more in each iteration.
# In purrr we iterate 3 times (map(), map(), map_dbl()), with apply functions
# we iterate twice (lapply(), vapply()), and with a for loop we iterate once.
# I prefer more, but simpler, steps because I think it makes the code easier
# to understand and later modify.

#  9.4 Map variants 
# There are 23 primary variants of map(). So far, you’ve learned about five (map(),
# map_lgl(), map_int(), map_dbl() and map_chr()). That means that you’ve got 18 (!!)
# more to learn. That sounds like a lot, but fortunately the design of purrr means
# that you only need to learn five new ideas:

# Output same type as input with modify()
# Iterate over two inputs with map2().
# Iterate with an index using imap()
# Return nothing with walk().
# Iterate over any number of inputs with pmap().

# The map family of functions has orthogonal input and outputs, meaning that we
# can organise all the family into a matrix, with inputs in the rows and outputs
# in the columns.

#  9.4.1 Same type of output as input: modify() 
# Imagine you wanted to double every column in a data frame.
# You might first try using map(), but map() always returns a list:
df <- data.frame(
    x = 1:3,
    y = 6:4,
    z = 5:7
)
df

map(df, ~ .x * .x)

# If you want to keep the output as a data frame, you can use modify(),
# which always returns the same type of output as the input:

modify(df, ~ .x * .x)

# Despite the name, modify() doesn’t modify in place, it returns a modified copy, 
# so if you wanted to permanently modify df, you’d need to assign it:
df <- modify(df, ~ .x * .x)
df

# As usual, the basic implementation of modify() is simple, and in fact it’s
# even simpler than map() because we don’t need to create a new output vector;
# we can just progressively replace the input. (The real code is a little complex
# to handle edge cases more gracefully.)
simple_modify <- function(x, f, ...) {
    for (i in seq_along(x)) {
        x[[i]] <- f(x[[i]], ...)
    }
    x
}
simple_modify(df, function(x) x * x)

#  9.4.2 Two inputs: map2() and friends 
# map() is vectorised over a single argument, .x. This means it only varies .x
# when calling .f, and all other arguments are passed along unchanged, thus making
# it poorly suited for some problems. For example, how would you find a weighted
# mean when you have a list of observations and a list of weights? 
# Imagine we have the following data:
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
xs
ws <- map(1:8, ~ rpois(10, 5) + 1)
ws

# You can use map_dbl() to compute the unweighted means:
map(xs, mean)
map_dbl(xs, mean)

# But passing ws as an additional argument doesn’t work because arguments after .f are not transformed:
map_dbl(xs, weighted.mean, w = ws)
#> Error in weighted.mean.default(.x[[i]], ...): 'x' and 'w' must have the same
#> length
#> 

# We need a new tool: a map2(), which is vectorised over two arguments.
# This means both .x and .y are varied in each call to .f:
map2_dbl(xs, ws, weighted.mean)

# The arguments to map2() are slightly different to the arguments to
# map() as two vectors come before the function, rather than one.
# Additional arguments still go afterwards:
map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)

# The basic implementation of map2() is simple, and quite similar
# to that of map(). Instead of iterating over one vector, we iterate over two in parallel:
simple_map2 <- function(x, y, f, ...) {
    out <- vector('list', length(x))
    for (i in seq_along(x)) {
        out[[i]] <- f(x[[i]], y[[i]], ...)
    }
    out
}
simple_map2(xs, ws, weighted.mean, na.rm = TRUE)

simple_map2_dbl <- function(x, y, f, ...) {
    out <- vector('double', length(x))
    for (i in seq_along(x)) {
        out[[i]] <- f(x[[i]], y[[i]], ...)
    }
    out
}

# One of the big differences between map2() and the simple
# function above is that map2() recycles its inputs to make sure
# that they’re the same length:

# In other words, map2(x, y, f) will automatically behave like map(x, f, y)
# when needed. This is helpful when writing functions; in scripts you’d
# generally just use the simpler form directly.

# 9.4.3 No outputs: walk() and friends 
# some functions do not return values or are not useful to us:

welcome <- function(x) {
    cat('Welcome', x, '!\n', sep = '')
}

names <- c('Marco', 'Hadley')

map(names, welcome)
# You could avoid this problem by assigning the results of map() to a
# variable that you never use, but that would muddy the intent of the code.
# Instead, purrr provides the walk family of functions that ignore the return
# values of the .f and instead return .x invisibly55.

walk(names, welcome)
# the outputs are ephemeral, and the input is returned invisibly.

# One of the most useful walk() variants is walk2() because a very common
# side-effect is saving something to disk, and when saving something to
# disk you always have a pair of values: the object and the path that you want to save it to.

# For example, imagine you have a list of data frames 
# and you’d like to save each one to a separate CSV file. That’s easy with walk2():
temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0('cyl-', names(cyls), '.csv'))
walk2(cyls, paths, write.csv)

dir(temp)

#  9.4.4 Iterating over values and indices 
# There are three basic ways to loop over a vector with a for loop:
#
# loop over the elements; `for (x in xs)`
# loop over the numeric indices: `for (i in seq_along(xs))`
# loop over the names: `for (nm in names(xs))`

# The first form is analogous to the map() family. The
# second and third forms are equivalent to the imap()
# family which allows you to iterate over the values and
# the indices of a vector in parallel.

# imap() is like map2() in the sense that your .f gets called
# with two arguments, but here both are derived from the vector.

# imap(x, f) is equivalent to map2(x, names(x), f) if x has names,
# and map2(x, seq_along(x), f) if it does not.

# imap() is often useful for constructing labels:
imap_chr(iris, ~ paste0('The first value of ', .y, ' is ', .x[[1]]))

# If the vector is unnamed, the second argument will be the index:
x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0('the highest value of ', .y, ' is ', max(.x)))

# imap() is a useful helper if you want to work with the values in a vector along with their positions.


#  9.4.5 Any number of inputs: pmap() and friends 
# Since we have map() and map2(), you might expect map3(), map4(),
# map5(), … But where would you stop? Instead of generalising map2() to
# an arbitrary number of arguments, purrr takes a slightly different tack
# with pmap(): you supply it a single list, which contains any number of
# arguments. In most cases, that will be a list of equal-length vectors,
# i.e. something very similar to a data frame. 

# There’s a simple equivalence between map2() and pmap(): map2(x, y, f)
# is the same as pmap(list(x, y), f).

# The pmap() equivalent to the map2_dbl(xs, ws, weighted.mean) used above is:
pmap_dbl(list(xs, ws), weighted.mean)

# As before, the varying arguments come before .f (although now they must be wrapped in a list),
# and the constant arguments come afterwards.
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)


# A big difference between pmap() and the other map functions is that
# pmap() gives you much finer control over argument matching because
# you can name the components of the list. Returning to our example from
# Section 9.2.5, where we wanted to vary the trim argument to x, we
# could instead use pmap():
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)


# draw random uniform numbers with varying parameters:
params <- tibble::tribble(
    ~ n, ~ min, ~ max,
     1L,     0,     1,
     2L,    10,   100,
     3L,   100,  1000,
     4L,  1000, 10000
)
pmap(params, runif)


# There are two base equivalents to the pmap() family: Map() and
# mapply(). Both have significant drawbacks:
# Map() vectorises over all arguments so you cannot supply arguments that do not vary.
# mapply() is the multidimensional version of sapply(); conceptually it
# takes the output of Map() and simplifies it if possible.
# This gives it similar issues to sapply(). There is no multi-input
# equivalent of vapply().


#  9.5 Reduce family 
# After the map family, the next most important family of functions is
# the reduce family. This family is much smaller, with only two main
# variants, and is used less commonly, but it’s a powerful idea,
# gives us the opportunity to discuss some useful algebra, and powers
# the map-reduce framework frequently used for processing very large datasets.

#  9.5.1 Basics 
# reduce() takes a vector of length n and produces a vector of length 1 by
# calling a function with a pair of values at a time: reduce(1:4, f)
# is equivalent to f(f(f(1, 2), 3), 4).
# reduce() takes a vector of length n and produces a vector of
# length 1 by calling a function with a pair of values at a time:
# reduce(1:4, f) is equivalent to f(f(f(1, 2), 3), 4).

# reduce() is a useful way to generalise a function that works with two
# inputs (a binary function) to work with any number of inputs.
# Imagine you have a list of numeric vectors, and you want to find
# the values that occur in every element. First we generate some sample data:
l <- map(1:4, ~ sample(1:10, 15, replace = TRUE))
str(l)

# To solve this challenge we need to use intersect() repeatedly:
out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
out

# reduce() automates this solution for us, so we can write:
reduce(l, intersect)

# We could apply the same idea if we wanted to list all the elements
# that appear in at least one entry. All we have to do is
# switch from intersect() to union():
reduce(l, union)

# As usual, the essence of reduce() can be reduced to a simple
# wrapper around a for loop:
simple_reduce <- function(x, f) {
    out <- x[[1]]
    for (i in seq(2, length(x))) {
        out <- f(out, x[[i]])
    }
    out
}
simple_reduce(l, union)

# The base equivalent is Reduce(). Note that the argument order
# is different: the function comes first, followed by the vector,
# and there is no way to supply additional arguments.


#  9.5.2 Accumulate 
# The first reduce() variant, accumulate(), is useful for understanding
# how reduce works, because instead of returning just the final result,
# it returns all the intermediate results as well:
accumulate(l, intersect)

# Another useful way to understand reduce is to think about sum():
# sum(x) is equivalent to x[[1]] + x[[2]] + x[[3]] + ..., i.e.
# reduce(x, `+`). Then accumulate(x, `+`) is the cumulative sum:
x <- 1:10
reduce(x, `+`)

accumulate(x, `+`)

#  9.5.3 Output types 
# In the above example using +, what should reduce() return when x
# is short, i.e. length 1 or 0? Without additional arguments,
# reduce() just returns the input when x is length 1:
reduce(1, `+`)

#This means that reduce() has no way to check that the input is valid:
reduce('a', `+`)

# What if it’s length 0? We get an error that suggests we need to
# use the .init argument:
reduce(integer(), `+`)


# What should .init be here? To figure that out, we need to
# see what happens when .init is supplied:
# So if we call reduce(1, `+`, init) the result will be 1 + init.
# Now we know that the result should be just 1, so that suggests
# that .init should be 0:
reduce(integer(), `+`, .init = 0)

# This also ensures that reduce() checks that length 1 inputs are
# valid for the function that you’re calling:
reduce('a', `+`, .init = 0)

# If you want to get algebraic about it, 0 is called the identity
# of the real numbers under the operation of addition: if you add
# a 0 to any number, you get the same number back. R applies the
# same principle to determine what a summary function with a zero
# length input should return:
sum(integer())  # x + 0 = x
prod(integer()) # x * 1 = x
min(integer())  # min(x, Inf) = x
max(integer())  # max(x, -Inf)


# If you’re using reduce() in a function, you should always
# supply .init. Think carefully about what your function should
# return when you pass a vector of length 0 or 1, and make sure
# to test your implementation.

#  9.5.4 Multiple inputs 
# Very occasionally you need to pass two arguments to the function
# that you’re reducing. For example, you might have a list
# of data frames that you want to join together, and the
# variables you use to join will vary from element to element.
# This is a very specialised scenario, so I don’t want to spend
# much time on it, but I do want you to know that reduce2() exists.

# The length of the second argument varies based on whether or
# not .init is supplied: if you have four elements of x, f will
# only be called three times. If you supply init, f will be
# called four times.

#  9.5.5 Map-reduce 
# You might have heard of map-reduce, the idea that powers technology
# like Hadoop. Now you can see how simple and powerful the
# underlying idea is: map-reduce is a map combined with a reduce.
# The difference for large data is that the data is spread over
# multiple computers. Each computer performs the map on the data
# that it has, then it sends the result to back to a coordinator
# which reduces the individual results back to a single result.

# As a simple example, imagine computing the mean of a very large vector,
# so large that it has to be split over multiple computers.
# You could ask each computer to calculate the sum and the length, and
# then return those to the coordinator which computes the overall mean
# by dividing the total sum by the total length.


#  9.6 Predicate functionals 
# A predicate is a function that returns a single TRUE or FALSE,
# like is.character(), is.null(), or all(), and we say a
# predicate matches a vector if it returns TRUE.

#  9.6.1 Basics
# A predicate functional applies a predicate to each
# element of a vector. purrr provides seven useful functions
# which come in three groups:
#   *   `some(.x, .p)` returns TRUE if any element matches
#               returns TRUE when it sees the first TRUE
#       `every(.x, .p)` returns TRUE if all elements match
#               return FALSE when it sees the first FALSE
#       `none(.x, .p)` returns TRUE if no element matches
#               return FALSE when it sees the first TRUE
#   *   `detect(.x, .p)` returns the value of the first match
#       `detect_index(.x, .p) returns the location of the first match.`
#
#   *   `keep(.x, .p)` keeps all matching elements
#       `discard(.x, .p)` drops all matching elements

df <- data.frame(x = 1:3,
                 y = c('a', 'b', 'c'))
detect(df, is.factor)
detect_index(df, is.factor)

str(keep(df, is.factor))
str(discard(df, is.factor))

#  9.6.2 Map variants 
# map() and modify() come in variants that also take predicate
# functions, transforming only the elements of .x where .p is TRUE.
df <- data.frame(
    num1 = c(0, 10, 20),
    num2 = c(5, 6, 7),
    chr1 = c('a', 'b', 'c'),
    stringsAsFactors = FALSE
)

str(map_if(df, is.numeric, mean))
str(modify_if(df, is.numeric, mean))

str(map(keep(df, is.numeric), mean))


#  9.7 Base functionals 

#  9.7.1 Matrices and arrays
# map() and friends are specialised to work with one-dimensiona
# vectors. base::apply() is specialised to work with two-dimensional and
# higher vectors, i.e. matrices and arrays. You can think of apply()
# as an operation that summarises a matrix or array by collapsing
# each row or column to a single value. It has four arguments:
#       * X, the matrix or array to summarise
#       * MARGIN, an integer vector giving the dimensions
#           to summarise over, 1 = rows, 2 = columns,
#           etc. (The argument name comes from thinking
#           about the margins of a joint distribution.)
#       * FUN, a summary function.
#       * ... other arguments passed on to FUN.

a2d <- matrix(1:20, nrow = 5)
a2d
apply(a2d, 1, mean)
apply(a2d, 2, mean)

# You can specify multiple dimensions to MARGIN, which is useful for high-dimensional arrays:
a3d <- array(1:24, c(2, 3, 4))
a3d
apply(a3d, 3, mean)


# There are two caveats to using apply():
#   Like base::sapply(), you have no control over the output type;
#       it will automatically be simplified to a list, matrix, or vector.
#       However, you usually use apply() with numeric arrays and a numeric
#       summary function so you are less likely to encounter a problem than with sapply().
#   apply() is also not idempotent in the sense that if the summary function is the
#       identity operator, the output is not always the same as the input.

a1 <- apply(a2d, 1, identity)
identical(a2d, a1)

a2 <- apply(a2d, 2, identity)
identical(a2d, a2)

# Never use apply() with a data frame. It always coerces it to a matrix,
# which will lead to undesirable results if your data frame contains
# anything other than numbers.
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
apply(df, 2, mean)

#  9.7.2 Mathematical concerns 
# 
# Functionals are very common in mathematics. The limit, the maximum,
# the roots (the set of points where f(x) = 0), and the definite integral
# are all functionals: given a function, they return a single number
# (or vector of numbers). At first glance, these functions don’t seem to
# fit in with the theme of eliminating loops, but if you dig deeper
# you’ll find out that they are all implemented using an algorithm
# that involves iteration.

# integrate() finds the area under the curve defined by f() 
integrate(sin, 0, pi)

#  uniroot() finds where f() hits zero
str(uniroot(sin, pi * c(1 / 2, 3 / 2)))

#  optimise() finds the location of the lowest (or highest) value of f() 
str(optimise(sin, c(0, 2 * pi)))
str(optimise(sin, c(0, pi), maximum = TRUE))
