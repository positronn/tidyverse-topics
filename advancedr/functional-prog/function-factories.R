# function-factories.R
library(purrr)

#  10.1 Introduction

# A function factory is a function that makes functions.
# Here’s a very simple example: we use a function factory
# (power1()) to make two child functions (square() and cube()):

power1 <- function(exp) {
    function(x) {
        x ^ exp
    }
}
square <- power1(2)
cube <- power1(3)

square(3)
cube(3)

# In R, you bind a function to a name in the same way as you bind any
# object to a name: with <-.
# a function captures (encloses) the environment in which it is created.
# a function creates a new execution environment every time it is run.
# This environment is usually ephemeral, but here it becomes the
# enclosing environment of the manufactured function.

library(rlang)
library(ggplot2)
library(scales)

# The enclosing environment of the manufactured function is an
# execution environment of the function factory.

#  10.2.1 Environments 
square

cube

# It’s obvious where x comes from, but how does R find the value
# associated with exp? Simply printing the manufactured functions
# is not revealing because the bodies are identical; the contents
# of the enclosing environment are the important factors. 

#  We can get a little more insight by using rlang::env_print().
# That shows us that we have two different environments (each of
# which was originally an execution environment of power1()). The
# environments have the same parent, which is the enclosing
# environment of power1(), the global environment.
env_print(square)
env_print(cube)

# env_print() shows us that both environments have a binding
# to exp, but we want to see its value. We can do that by first
# getting the environment of the function, and then extracting the values:
fn_env(square)$exp
fn_env(cube)$exp

# This is what makes manufactured functions behave differently
# from one another: names in the enclosing environment are bound
# to different values.

#  10.2.3 Forcing evaluation
# There’s a subtle bug in power1() caused by lazy evaluation.
# To see the problem we need to introduce some indirection:
x <- 2
square <- power1(x)
x <- 3

# What should square(2) return? You would hope it returns 4:
square(2)

# Unfortunately it doesn’t because x is only evaluated lazily
# when square() is run, not when power1() is run. In general,
# this problem will arise whenever a binding changes in between
# calling the factory function and calling the manufactured function.
# This is likely to only happen rarely, but when it does,
# it will lead to a real head-scratcher of a bug.

# We can fix this problem by forcing evaluation with force():
power2 <- function(exp) {
    force(exp)
    function(x) {
        x ^ exp
    }
}

x <- 2
square <- power2(x)
x <- 3
square(2)


# Whenever you create a function factory, make sure every argument
# is evaluated, using force() as necessary if the argument
# is only used by the manufactured function.

#  10.2.4 Stateful functions 
# Function factories also allow you to maintain state across function invocations
# There are two things that make this possible:
# The enclosing environment of the manufactured function is unique and constant.
# R has a special assignment operator, <<-, which modifies bindings in the enclosing environment.

# The usual assignment operator, <-, always creates a binding in the
# current environment. The super assignment operator,
# <<- rebinds an existing name found in a parent environment.

# The following example shows how we can combine these ideas to
# create a function that records how many times it has been called:
new_counter <- function() {
    i <- 0
    function() {
        i <<- i + 1
        i
    }
}

# When the manufactured function is run i <<- i + 1 will modify i in
# its enclosing environment. Because manufactured functions have
# independent enclosing environments, they have independent counts:

counter_one <- new_counter()
counter_two <- new_counter()


# Stateful functions are best used in moderation. As soon as your
# function starts managing the state of multiple variables, it’s better
# to switch to R6, the topic of Chapter 14.


#  10.2.5 Garbage collection 
# With most functions, you can rely on the garbage collector to 
# clean up any large temporary objects created inside a function.
# However, manufactured functions hold on to the execution environment,
# so you’ll need to explicitly unbind any large temporary objects with rm().
# Compare the sizes of g1() and g2() in the example below:
f1 <- function(n) {
    x <- runif(n)
    m <- mean(x)
    function() m
}
g1 <- f1(1e6)
lobstr::obj_size(g1)



f2 <- function(n) {
    x <- runif(n)
    m <- mean(x)
    rm(x)
    function() m
}
g2 <- f2(1e6)
lobstr::obj_size(g2)


# Create a function pick() that takes an index, i, as an
# argument and returns a function with an argument x that subsets x with i.
x <- c(0, 1, 2, 3)
pick <- function(i) {
    function(x) {
        x[[i]]
    }
}
pick(1)(x)
x[[1]]

lapply(mtcars, pick(5))
lapply(mtcars, function(x) x[[5]])


# What happens if you use <- instead of <<-? Make predictions,
# then verify with the code below.
# if <- is used instead of <<-, i will always go back to its initial
# state when the function was created, i.e. i = 0, and the counter
# will always return 1
new_counter3 <- function() {
    i <- 0
    function() {
        i <<- i + 1
        i
    }
}

nc3 <- new_counter3()
nc3()
nc3()
nc3()


new_counter3 <- function() {
    i <- 0
    function() {
        i <- i + 1
        i
    }
}
nc3 <- new_counter3()
nc3()
nc3()
nc3()


#  10.3 Graphical factories 
# We’ll begin our exploration of useful function factories with a few examples from ggplot2.
# 10.3.1 Labelling 
library(ggplot2)
library(scales)
y <- c(12345, 123456, 1234567)

comma_format()(y)
number_format(scale = 1e-3, suffix = " K")(y)

df <- data.frame(x = 1, y = y)
core <- ggplot(df, aes(x, y)) + 
    geom_point() + 
    scale_x_continuous(breaks = 1, labels = NULL) +
    labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
    labels = comma_format()
)
core + scale_y_continuous(
    labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
    labels = scientific_format()
)


#  10.3.2 Histogram bins 
# A little known feature of geom_histogram() is that the binwidth argument can
# be a function. This is particularly useful because the function is executed once
# for each group, which means you can have different binwidths in different facets,
# which is otherwise not possible.
# construct some sample data with very different numbers in each cell
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd),
                 sd = rep(sd, n))

ggplot(df, aes(x)) + 
    geom_histogram(binwidth = 2) + 
    facet_wrap(~ sd, scales = "free_x") + 
    labs(x = NULL)

bindwidth_bins <- function(n) {
    force(n)
    function(x) {
        (max(x) - min(x)) / n
    }
}

ggplot(df, aes(x)) +
    geom_histogram(binwidth = bindwidth_bins(20)) +
    facet_wrap(~ sd, scales = 'free_x') +
    labs(x = NULL)

# We could use this same pattern to wrap around the base R functions
# that automatically find the so-called optimal58 binwidth, nclass.Sturges(),
# nclass.scott(), and nclass.FD()
base_bins <- function(type) {
    fun <- switch(
        type,
        Sturges = nclass.Sturges,
        scott = nclass.scott,
        FD = nclass.FD,
        stop('Unkown type', call. = FALSE)
    )
    function(x) {
        (max(x) - min(x)) / fun(x)
    }
}

ggplot(df, aes(x)) +
    geom_histogram(binwidth = base_bins('Sturges')) +
    facet_wrap(~ sd, scales = 'free_x') +
    labs(x = NULL)


#  10.3.3 ggsave() 
# ggplot2:::plot_dev() is used by ggsave() to go from a file extension
# (e.g. png, jpeg etc) to a graphics device function (e.g. png(), jpeg()).
# The challenge here arises because the base graphics devices have some minor
# inconsistencies which we need to paper over:
# * Most have filename as first argument but some have file.
# * The width and height of raster graphic devices use pixels units by default,
#       but the vector graphics use inches.
# A mildly simplified version of plot_dev() is shown below:
plot_dev <- function(ext, dpi = 96) {
    force(dpi)
    switch(
        ext,
        eps = ,
        ps = function(path, ...) {
            grDevices::postscript(
                file = filename, ..., onefile = FALSE,
                horizontal = FALSE, paper = 'special'
            )
        },
        pdf = function(filename, ...) {
            grDevices::pdf(file = filename, ...)
        },
        svg = function(filename, ...) {
            svglite::svglite(file = filename, ...)
        },
        emf = ,
        wmf = function(...) {
            grDevices::win.metafile(...)
        },
        png = function(...) {
            grDevices::png(..., res = dpi, units = 'in')
        },
        jpg = ,
        jpeg = function(...) {
            grDevices::jpeg(..., res = dpi, units = 'in')
        },
        bmp = function(...) {
            grDevices::bmp(..., res = dpi, units = 'in')
        },
        tiff = function(...) {
            grDevices::tiff(..., res = dpi, units = 'in')
        },
        stop('Unkown graphics extension: ', ext, call. = FALSE)
    )
}

plot_dev('pdf')
plot_dev("png")


#  10.4 Statistical factories 
# More motivating examples for function factories come from statistics:
#       The Box-Cox transformation.
#       Bootstrap resampling.
#       Maximum likelihood estimation.

#  10.4.1 Box-Cox transformation 

# The Box-Cox transformation (a type of power transformation) is a flexible
# transformation often used to transform data towards normality. It has a single parameter
# , λ, which controls the strength of the transformation. We could express the
# transformation as a simple two argument function:
boxcox1 <- function(x, lambda) {
    stopifnot(length(lambda) == 1)
    if (lambda == 0) {
        log(x)
    }
    else {
        (x ^ lamda - 1) / lambda
    }
}

# But re-formulating as a function factory makes it easy to explore its behaviour with stat_function():
boxcox2 <- function(lambda) {
    if (lambda == 0) {
        function(x) log(x)
    }
    else {
        function(x) (x ^ lambda - 1) / lambda
    }
}

stat_boxcox <- function(lambda) {
    stat_function(aes(colour = lambda),
                  fun = boxcox2(lambda),
                  size = 1)
}

ggplot(data.frame(x = c(0, 5)),
       aes(x)) +
    lapply(c(0.5, 1, 1.5), stat_boxcox) +
    scale_colour_viridis_c(limits = c(0, 1.5))


# visually, log() does seem to make sense as the ransformation
# for lambda = 0; as values get smaller and smaller, the function
# gets close and closer to a log transformation
ggplot(data.frame(x = c(0.01, 1)), aes(x)) +
    lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) +
    scale_colour_viridis_c(limits = c(0, 1.5))




#  10.4.2 Bootstrap generators 
# Function factories are a useful approach for bootstrapping.
# Instead of thinking about a single bootstrap (you always need more
# than one!), you can think about a bootstrap generator, a
# mfunction that yields a fresh bootstrap every time it is called:
boot_permute <- function(df, var) {
    n <- nrow(df)
    force(var)
    function() {
        col <- df[[var]]
        col[sample(n, replace = TRUE)]
    }
}

boot_mtcars1 <- boot_permute(mtcars, 'mpg')
head(boot_mtcars1())
head(boot_mtcars1())
head(boot_mtcars1())


# The advantage of a function factory is more clear with a
# parametric bootstrap where we have to first fit a model.
# We can do this setup step once, when the factory is called,
# rather than once every time we generate the bootstrap:
boot_model <- function(df, formula) {
    mod <- lm(formula, data = df)
    fitted <- unname(fitted(mod))
    resid <- unname(resid(mod))
    rm(mod)
    # use rm(mod) because linear model objects are quite
    # large (they include complete copies of the model matrix
    # and input data) 
    function(x) {
        fitted + sample(resid)
    }
}

boot_mtcars2 <- boot_model(mtcars, mpg ~ wt)
head(boot_mtcars2())

#  10.4.3 Maximum likelihood estimation 
# The goal of maximum likelihood estimation (MLE) is to find
# the parameter values for a distribution that make the observed data most likely.
lprob_poisson <- function(lambda, x) {
    n <- length(x)
    (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}
# Consider this vector of observations:
x1 <- c(41, 30, 31, 38, 29, 24, 30, 29, 31, 38)

# We can use lprob_poisson() to compute the (logged) probability of x1 for different values of lambda.
lprob_poisson(10, x1)
lprob_poisson(20, x1)
lprob_poisson(30, x1)


# So far we’ve been thinking of lambda as fixed and known and the function
# told us the probability of getting different values of x. But in real-life,
# we observe x and it is lambda that is unknown. The likelihood is the
# probability function seen through this lens: we want to find the lambda
# that makes the observed x the most likely. That is, given x, what value of
# lambda gives us the highest value of lprob_poisson()?

# we can use a function factory. We provide x and generate a function
# with a single parameter, lambda:
ll_poisson1 <- function(x) {
    n <- length(x)
    function(lambda) {
        log(lambda) * sum(x) - n * lambda - sum(lfactorial(x))
    }
}
# (We don’t need force() because length() implicitly forces evaluation of x.)
# One nice thing about this approach is that we can do some precomputation:
# any term that only involves x can be computed once in the factory.
# This is useful because we’re going to need to call this function many
# times to find the best lambda.
ll_poisson2 <- function(x) {
    n <- length(x)
    sum_x <- sum(x)
    c <- sum(lfactorial(x))
    function(lambda) {
        log(lambda) * sum_x - n * lambda - c
    }
}

# Now we can use this function to find the value of lambda that maximizes
# the (log) likelihood:
ll1 <- ll_poisson2(x1)
ll1(10)
ll1(20)
ll1(30)

# Rather than trial and error, we can automate the process of finding
# the best value with optimise().
#  It will evaluate ll1() many times, using mathematical tricks
# to narrow in on the largest value as quickly as possible. The results
# tell us that the highest value is -30.27 which occurs when lambda = 32.1:
optimise(ll1, c(0, 100), maximum = TRUE)


# Now, we could have solved this problem without using a function
# factory because optimise() passes ... on to the function being
# optimised. That means we could use the log-probability function directly:
optimise(lprob_poisson, c(0, 100), x = x1, maximum = TRUE)



# The advantage of using a function factory here is fairly
# small, but there are two niceties:
# We can precompute some values in the factory, saving
# computation time in each iteration.
# The two-level design better reflects the mathematical
# structure of the underlying problem.
# These advantages get bigger in more complex MLE problems, where you have
# multiple parameters and multiple data vectors.


# 10.5 Function factories + functionals 
# To finish off the chapter, I’ll show how you might combine functionals
# and function factories to turn data into many functions. The following code
# creates many specially named power functions by iterating over a list of arguments:
names <- list(
    square = 2,
    cube = 3,
    root = 1/2,
    cuberoot = 1/2,
    reciprocal = -1
)
funs <- purrr::map(names, power1)

funs$root(64)
funs$root


# This idea extends in a straightforward way if your
# function factory takes two (replace map() with map2()) or more (replace with pmap()) arguments.
# One downside of the current construction is that you have
# to prefix every function call with funs$. There are three ways to eliminate this additional syntax:
with(funs, root(100))

# I recommend this because it makes it very clear when
# code is being executed in a special context and what that context is.

# For a longer effect, you can attach() the functions to the
# search path, then detach() when you’re done:
attach(funs)
root(100)
detach(funs)
